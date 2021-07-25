import { readLines } from "https://deno.land/std@0.102.0/io/bufio.ts";
import { expr, stmt } from "./ast.ts";

type Expr = expr.Expr;
type Stmt = stmt.Stmt;

export enum TokenType {
  // Single-character tokens.
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,

  // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,

  // Literals.
  IDENTIFIER,
  STRING,
  NUMBER,

  // Keywords.
  AND,
  CLASS,
  ELSE,
  FALSE,
  FUN,
  FOR,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,

  EOF,
}

const TT = TokenType;

export class Token {
  typ: TokenType;
  lexeme: string;
  literal: any;
  line: number;

  constructor(typ: TokenType, lexeme: string, literal: any, line: number) {
    this.typ = typ;
    this.lexeme = lexeme;
    this.literal = literal;
    this.line = line;
  }

  public toString(): string {
    return TT[this.typ] + " " + this.lexeme + " " + this.literal;
  }
}

class ParseError extends Error {}

class Parser {
  tokens: Token[];
  private current: number = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  parse(): Stmt[] {
    const statements: Stmt[] = [];
    while (!this.isAtEnd()) {
      const decl = this.declaration();
      if (decl) {
        statements.push(decl);
      }
    }
    return statements;
  }

  private expression(): Expr {
    return this.assignment();
  }

  private declaration(): Stmt | null {
    try {
      if (this.match(TT.VAR)) return this.varDeclaration();
      return this.statement();
    } catch (err) {
      if (err instanceof ParseError) {
        this.synchronize();
        return null;
      } else {
        throw err;
      }
    }
  }

  private statement(): Stmt {
    if (this.match(TT.IF)) return this.ifStatement();
    if (this.match(TT.PRINT)) return this.printStatement();
    if (this.match(TT.LEFT_BRACE)) return new stmt.Block(this.block());
    return this.expressionStatement();
  }

  private ifStatement(): Stmt {
    this.consume(TT.LEFT_PAREN, "Expect '(' after 'if'.");
    const condition = this.expression();
    this.consume(TT.RIGHT_PAREN, "Expect ')' after if condition.");
    const thenBranch = this.statement();
    let elseBranch = null;
    if (this.match(TT.ELSE)) {
      elseBranch = this.statement();
    }
    return new stmt.If(condition, thenBranch, elseBranch);
  }

  private printStatement(): Stmt {
    const value = this.expression();
    this.consume(TT.SEMICOLON, "Expect ';' after value.");
    return new stmt.Print(value);
  }

  private varDeclaration() {
    const name = this.consume(TT.IDENTIFIER, "Expect variable name.");
    let initializer = null;
    if (this.match(TT.EQUAL)) {
      initializer = this.expression();
    }
    this.consume(TT.SEMICOLON, "Expect ';' after variable declaration.");
    return new stmt.Var(name, initializer);
  }

  private expressionStatement(): Stmt {
    const exp = this.expression();
    this.consume(TT.SEMICOLON, "Expect ';' after expression.");
    return new stmt.Expression(exp);
  }

  private block(): Stmt[] {
    const statements = [];
    while (!this.check(TT.RIGHT_BRACE) && !this.isAtEnd()) {
      const decl = this.declaration();
      if (decl) {
        statements.push(decl);
      }
    }
    this.consume(TT.RIGHT_BRACE, "Expect '}' after block.");
    return statements;
  }

  private assignment(): Expr {
    const exp = this.or();
    if (this.match(TT.EQUAL)) {
      const equals = this.previous();
      const value = this.assignment();
      if (exp instanceof expr.Variable) {
        const name = (exp as expr.Variable).name;
        return new expr.Assign(name, value);
      }
      this.error(equals, "Invalid assignment target.");
    }
    return exp;
  }

  private or(): Expr {
    let exp = this.and();
    while (this.match(TT.OR)) {
      const op = this.previous();
      const right = this.and();
      exp = new expr.Logical(exp, op, right);
    }
    return exp;
  }

  private and(): Expr {
    let exp = this.equality();
    while (this.match(TT.AND)) {
      const op = this.previous();
      const right = this.equality();
      exp = new expr.Logical(exp, op, right);
    }
    return exp;
  }

  private equality(): Expr {
    let exp = this.comparison();

    while (this.match(TT.BANG_EQUAL, TT.EQUAL_EQUAL)) {
      const operator = this.previous();
      const right = this.comparison();
      exp = new expr.Binary(exp, operator, right);
    }

    return exp;
  }

  private comparison(): Expr {
    let exp = this.term();
    while (this.match(TT.GREATER, TT.GREATER_EQUAL, TT.LESS, TT.LESS_EQUAL)) {
      const op = this.previous();
      const right = this.term();
      exp = new expr.Binary(exp, op, right);
    }
    return exp;
  }

  private term(): Expr {
    let exp = this.factor();
    while (this.match(TT.MINUS, TT.PLUS)) {
      const op = this.previous();
      const right = this.factor();
      exp = new expr.Binary(exp, op, right);
    }
    return exp;
  }

  private factor(): Expr {
    let exp = this.unary();
    while (this.match(TT.SLASH, TT.STAR)) {
      const op = this.previous();
      const right = this.unary();
      exp = new expr.Binary(exp, op, right);
    }
    return exp;
  }

  private unary(): Expr {
    if (this.match(TT.BANG, TT.MINUS)) {
      const op = this.previous();
      const right = this.unary();
      return new expr.Unary(op, right);
    }
    return this.primary();
  }

  private primary(): Expr {
    if (this.match(TT.FALSE)) return new expr.Literal(false);
    if (this.match(TT.TRUE)) return new expr.Literal(true);
    if (this.match(TT.NIL)) return new expr.Literal(null);

    if (this.match(TT.NUMBER, TT.STRING)) {
      return new expr.Literal(this.previous().literal);
    }

    if (this.match(TT.IDENTIFIER)) {
      return new expr.Variable(this.previous());
    }

    if (this.match(TT.LEFT_PAREN)) {
      const exp = this.expression();
      this.consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
      return new expr.Grouping(exp);
    }

    throw new ParseError();
  }

  private match(...types: TokenType[]): boolean {
    for (const typ of types) {
      if (this.check(typ)) {
        this.advance();
        return true;
      }
    }
    return false;
  }

  private consume(typ: TokenType, message: string): Token {
    if (this.check(typ)) return this.advance();
    throw this.error(this.peek(), message);
  }

  private check(typ: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().typ == typ;
  }

  private advance(): Token {
    if (!this.isAtEnd()) this.current++;
    return this.previous();
  }

  private isAtEnd(): boolean {
    return this.peek().typ == TT.EOF;
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private error(token: Token, message: string): ParseError {
    Lox.errorToken(token, message);
    return new ParseError();
  }

  private synchronize() {
    this.advance();
    while (!this.isAtEnd()) {
      if (this.previous().typ == TT.SEMICOLON) return;
      switch (this.peek().typ) {
        case TT.CLASS:
        case TT.FUN:
        case TT.VAR:
        case TT.FOR:
        case TT.IF:
        case TT.WHILE:
        case TT.PRINT:
        case TT.RETURN:
          return;
      }
      this.advance();
    }
  }
}

class Environment {
  enclosing: Environment | null;
  values: Map<string, any> = new Map<string, any>();
  // values: { [name: string] : any } = {};

  constructor(enclosing?: Environment) {
    if (enclosing) {
      this.enclosing = enclosing;
    } else {
      this.enclosing = null;
    }
  }

  define(name: string, value: any) {
    this.values.set(name, value);
    // this.values[name] = value;
  }

  get(name: Token): any {
    if (this.values.has(name.lexeme)) {
      return this.values.get(name.lexeme);
    }
    // if (name.lexeme in this.values) {
    //   return this.values[name.lexeme];
    // }
    if (this.enclosing !== null) {
      return this.enclosing.get(name);
    }
    throw new RuntimeError(name, `Undefined variable '${name.lexeme}'.`);
  }

  assign(name: Token, value: any): any {
    if (this.values.has(name.lexeme)) {
      this.values.set(name.lexeme, value);
      return;
    }
    if (this.enclosing !== null) {
      return this.enclosing.assign(name, value);
    }
    throw new RuntimeError(name, `Undefined variable '${name.lexeme}'.`);
  }
}

class RuntimeError extends Error {
  token: Token;

  constructor(token: Token, message: string) {
    super(message);
    this.token = token;
  }
}

class Interpreter implements expr.Visitor<any>, stmt.Visitor<void> {
  private environment: Environment = new Environment();

  interpret(statements: Stmt[]) {
    try {
      for (const statement of statements) {
        this.execute(statement);
      }
    } catch (err) {
      if (err instanceof RuntimeError) {
        Lox.runtimeError(err);
      } else {
        throw err;
      }
    }
  }

  private evaluate(exp: Expr): any {
    return exp.accept(this);
  }

  private execute(stm: Stmt) {
    stm.accept(this);
  }

  private executeBlock(statements: Stmt[], environment: Environment) {
    const previous = this.environment;
    try {
      this.environment = environment;
      for (const statement of statements) {
        this.execute(statement);
      }
    } finally {
      this.environment = previous;
    }
  }

  visitBlockStmt(stm: stmt.Block) {
    this.executeBlock(stm.statements, new Environment(this.environment));
  }

  visitExpressionStmt(stm: stmt.Expression) {
    this.evaluate(stm.expression);
  }

  visitIfStmt(stm: stmt.If) {
    if (this.isTruthy(this.evaluate(stm.condition))) {
      this.execute(stm.thenBranch);
    } else if (stm.elseBranch !== null) {
      this.execute(stm.elseBranch);
    }
  }

  visitPrintStmt(stm: stmt.Print) {
    const value = this.evaluate(stm.expression);
    Deno.stdout.write(ENC.encode(this.stringify(value) + "\n"));
  }

  visitVarStmt(stm: stmt.Var) {
    let value = null;
    if (stm.initializer != null) {
      value = this.evaluate(stm.initializer);
    }
    this.environment.define(stm.name.lexeme, value);
    return null;
  }

  visitAssignExpr(exp: expr.Assign): any {
    const value = this.evaluate(exp.value);
    this.environment.assign(exp.name, value);
    return value;
  }

  visitLiteralExpr(exp: expr.Literal): any {
    return exp.value;
  }

  visitLogicalExpr(exp: expr.Logical): any {
    const left = this.evaluate(exp.left);
    if (exp.operator.typ == TT.OR) {
      if (this.isTruthy(left)) return left;
    } else {
      if (!this.isTruthy(left)) return left;
    }
    return this.evaluate(exp.right);
  }

  visitBinaryExpr(exp: expr.Binary): any {
    const left = this.evaluate(exp.left);
    const right = this.evaluate(exp.right);
    switch (exp.operator.typ) {
      case TT.BANG_EQUAL:
        return !this.isEqual(left, right);
      case TT.EQUAL_EQUAL:
        return this.isEqual(left, right);
      case TT.GREATER:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) > (right as number);
      case TT.GREATER_EQUAL:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) >= (right as number);
      case TT.LESS:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) < (right as number);
      case TT.LESS_EQUAL:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) <= (right as number);
      case TT.MINUS:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) - (right as number);
      case TT.PLUS:
        if (typeof left === "number" && typeof right === "number") {
          // if (left instanceof Number && right instanceof Number) {
          return (left as number) + (right as number);
        }
        if (typeof left === "string" && typeof right === "string") {
          return (left as string) + (right as string);
        }
        throw new RuntimeError(
          exp.operator,
          "Operands must be two numbers or two strings.",
        );
      case TT.SLASH:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) / (right as number);
      case TT.STAR:
        this.checkNumberOperands(exp.operator, left, right);
        return (left as number) * (right as number);
    }
    // Unreachable.
    return null;
  }

  visitGroupingExpr(exp: expr.Grouping): any {
    return this.evaluate(exp.expression);
  }

  visitUnaryExpr(exp: expr.Unary): any {
    const right = this.evaluate(exp.right);
    switch (exp.operator.typ) {
      case TT.BANG:
        return !this.isTruthy(right);
      case TT.MINUS:
        this.checkNumberOperand(exp.operator, right);
        return -(right as number);
    }
    return null;
  }

  visitVariableExpr(exp: expr.Variable): any {
    return this.environment.get(exp.name);
  }

  private checkNumberOperand(op: Token, operand: any) {
    if (typeof operand === "number") return;
    throw new RuntimeError(op, "Operand must be a number.");
  }

  private checkNumberOperands(op: Token, left: any, right: any) {
    // if (!isNaN(left) instanceof Number && right instanceof Number) return;
    if (typeof left === "number" && typeof right === "number") return;
    throw new RuntimeError(op, "Operands must be numbers.");
  }

  private isTruthy(object: any): boolean {
    if (null === object || undefined === object || false === object) {
      return false;
    }
    if (object instanceof Boolean) return object as boolean;
    return true;
  }

  private isEqual(a: any, b: any): boolean {
    if (null == a && null == b) return true;
    if (null == a) return false;
    return a === b;
  }

  private stringify(object: any) {
    if (null === object) return "nil";
    if (typeof object === "number") {
      return JSON.stringify(object);
    }
    return JSON.stringify(object);
  }
}

class Scanner {
  source: string;
  tokens: Token[] = [];
  private start: number = 0;
  private current: number = 0;
  private line: number = 1;

  static keywords: { [keyword: string]: TokenType } = {
    "and": TT.AND,
    "class": TT.CLASS,
    "else": TT.ELSE,
    "false": TT.FALSE,
    "for": TT.FOR,
    "fun": TT.FUN,
    "if": TT.IF,
    "nil": TT.NIL,
    "or": TT.OR,
    "print": TT.PRINT,
    "return": TT.RETURN,
    "super": TT.SUPER,
    "this": TT.THIS,
    "true": TT.TRUE,
    "var": TT.VAR,
    "while": TT.WHILE,
  };

  constructor(source: string) {
    this.source = source;
  }

  scanTokens(): Token[] {
    while (!this.isAtEnd()) {
      this.start = this.current;
      this.scanToken();
    }

    this.tokens.push(new Token(TT.EOF, "", null, this.line));
    return this.tokens;
  }

  private scanToken() {
    const ch = this.advance();
    switch (ch) {
      case "(":
        this.addToken(TT.LEFT_PAREN);
        break;
      case ")":
        this.addToken(TT.RIGHT_PAREN);
        break;
      case "{":
        this.addToken(TT.LEFT_BRACE);
        break;
      case "}":
        this.addToken(TT.RIGHT_BRACE);
        break;
      case ",":
        this.addToken(TT.COMMA);
        break;
      case ".":
        this.addToken(TT.DOT);
        break;
      case "-":
        this.addToken(TT.MINUS);
        break;
      case "+":
        this.addToken(TT.PLUS);
        break;
      case ";":
        this.addToken(TT.SEMICOLON);
        break;
      case "*":
        this.addToken(TT.STAR);
        break;
      case "!":
        this.addToken(this.match("=") ? TT.BANG_EQUAL : TT.BANG);
        break;
      case "=":
        this.addToken(this.match("=") ? TT.EQUAL_EQUAL : TT.EQUAL);
        break;
      case "<":
        this.addToken(this.match("=") ? TT.LESS_EQUAL : TT.LESS);
        break;
      case ">":
        this.addToken(this.match("=") ? TT.GREATER_EQUAL : TT.GREATER);
        break;
      case "/":
        if (this.match("/")) {
          // A comment goes until the end of the line.
          while (this.peek() !== "\n" && !this.isAtEnd()) this.advance();
        } else {
          this.addToken(TT.SLASH);
        }
        break;
      case " ":
      case "\r":
      case "\t":
        // Ignore whitespace.
        break;

      case "\n":
        this.line++;
        break;
      case '"':
        this.string();
        break;
      default:
        if (this.isDigit(ch)) {
          this.number();
        } else if (this.isAlpha(ch)) {
          this.identifier();
        } else {
          Lox.error(this.line, "Unexpected character.");
        }
        break;
    }
  }

  private identifier() {
    while (this.isAlphaNumeric(this.peek())) this.advance();
    const text = this.source.substring(this.start, this.current);
    let typ: TokenType = Scanner.keywords[text];
    if (typ == null) typ = TT.IDENTIFIER;
    this.addToken(typ);
  }

  private number() {
    while (this.isDigit(this.peek())) this.advance();

    // Look for a fractional part.
    if (this.peek() == "." && this.isDigit(this.peekNext())) {
      // Consume the "."
      this.advance();

      while (this.isDigit(this.peek())) this.advance();
    }

    this.addTokenLiteral(
      TT.NUMBER,
      Number.parseFloat(this.source.substring(this.start, this.current)),
    );
  }

  private string() {
    while (this.peek() != '"' && !this.isAtEnd()) {
      if (this.peek() == "\n") this.line++;
      this.advance();
    }

    if (this.isAtEnd()) {
      Lox.error(this.line, "Unterminated string.");
      return;
    }

    // The closing ".
    this.advance();

    // Trim the surrounding quotes.
    const value = this.source.substring(this.start + 1, this.current - 1);
    this.addTokenLiteral(TT.STRING, value);
  }

  private match(expected: string): boolean {
    if (this.isAtEnd()) return false;
    if (this.source.charAt(this.current) !== expected) return false;
    this.current++;
    return true;
  }

  private peek(): string {
    if (this.isAtEnd()) return "\0";
    return this.source.charAt(this.current);
  }

  private peekNext(): string {
    if (this.current + 1 >= this.source.length) return "\0";
    return this.source.charAt(this.current + 1);
  }

  private isAlpha(c: string): boolean {
    return (c >= "a" && c <= "z") ||
      (c >= "A" && c <= "Z") ||
      c == "_";
  }

  private isAlphaNumeric(c: string): boolean {
    return this.isAlpha(c) || this.isDigit(c);
  }

  private isDigit(c: string): boolean {
    return c >= "0" && c <= "9";
  }

  private advance(): string {
    return this.source.charAt(this.current++);
  }

  private addToken(typ: TokenType) {
    this.addTokenLiteral(typ, null);
  }

  private addTokenLiteral(typ: TokenType, literal: any) {
    const text = this.source.substring(this.start, this.current);
    this.tokens.push(new Token(typ, text, literal, this.line));
  }

  isAtEnd(): boolean {
    return this.current >= this.source.length;
  }
}

/*
class Printer implements expr.Visitor<void> {
  visitLiteralExpr(exp: expr.Literal) {
    Deno.stdout.write(ENC.encode(`${exp.value} `));
  }
  visitBinaryExpr(exp: expr.Binary) {
    Deno.stdout.write(ENC.encode(`(${exp.operator.lexeme.toString()} `));
    exp.left.accept(this);
    exp.right.accept(this);
    Deno.stdout.write(ENC.encode(`)`));
  }
  visitGroupingExpr(exp: expr.Grouping) {
    exp.expression.accept(this);
  }
  visitUnaryExpr(exp: expr.Unary) {
    Deno.stdout.write(ENC.encode(exp.operator.toString()));
    exp.right.accept(this);
  }
}
/**/

module Lox {
  export let interpreter: Interpreter = new Interpreter();
  export let hadError: boolean = false;
  export let hadRuntimeError: boolean = false;

  export const report = (line: number, where: string, message: string) => {
    console.error(`[line ${line}] Error${where}: ${message}`);
    hadError = true;
  };

  export const error = (line: number, message: string) => {
    report(line, "", message);
  };

  export const errorToken = (token: Token, message: string) => {
    if (token.typ == TT.EOF) {
      report(token.line, " at end", message);
    } else {
      report(token.line, ` at '${token.lexeme}'`, message);
    }
  };

  export const runtimeError = (err: RuntimeError) => {
    console.error(err.message + `\n[line ${err.token.line}]`);
    hadRuntimeError = true;
  };
}

const ENC = new TextEncoder();

function run(source: string) {
  const scanner = new Scanner(source);
  const tokens = scanner.scanTokens();
  if (Lox.hadError) return;

  const parser = new Parser(tokens);
  const exp = parser.parse();

  if (Lox.hadError) return;

  Lox.interpreter.interpret(exp);
}

async function runFile(file: string) {
  const contents = await Deno.readTextFile(file);
  run(contents);

  if (Lox.hadError) Deno.exit(65);
  if (Lox.hadRuntimeError) Deno.exit(70);
}

async function runPrompt() {
  Deno.stdout.write(ENC.encode("> "));
  for await (const line of readLines(Deno.stdin)) {
    console.log(line);
    Lox.hadError = false;

    Deno.stdout.write(ENC.encode("> "));
  }
}

{
  const args = Deno.args;
  if (args.length > 1) {
    console.log(`Usage: deno run --allow-read lox.ts [script]`);
    Deno.exit(64);
  } else if (args.length == 1) {
    await runFile(Deno.args[0]);
  } else {
    await runPrompt();
  }

  // const p = new Printer();
  // const l = new expr.Literal(5);
  // l.accept(p);
  // const parser = new Parser([new Token(TT.STRING, "hi", "hi", 1)]);
  // const exp = parser.parse();
  // exp.accept(p);
}
