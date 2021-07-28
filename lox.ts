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
      if (this.match(TT.CLASS)) return this.classDeclaration();
      if (this.match(TT.FUN)) return this.function("function");
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

  private classDeclaration(): Stmt {
    const name = this.consume(TT.IDENTIFIER, "Expect class name.");
    let superclass = null;
    if (this.match(TT.LESS)) {
      this.consume(TT.IDENTIFIER, "Expect superclass name.");
      superclass = new expr.Variable(this.previous());
    }

    this.consume(TT.LEFT_BRACE, "Expect '{' before class body.");

    const methods: stmt.Function[] = [];
    while (!this.check(TT.RIGHT_BRACE) && !this.isAtEnd()) {
      methods.push(this.function("method"));
    }
    this.consume(TT.RIGHT_BRACE, "Expect '}' after class body.");
    return new stmt.Class(name, superclass, methods);
  }

  private statement(): Stmt {
    if (this.match(TT.FOR)) return this.forStatement();
    if (this.match(TT.IF)) return this.ifStatement();
    if (this.match(TT.PRINT)) return this.printStatement();
    if (this.match(TT.RETURN)) return this.returnStatement();
    if (this.match(TT.WHILE)) return this.whileStatement();
    if (this.match(TT.LEFT_BRACE)) return new stmt.Block(this.block());
    return this.expressionStatement();
  }

  private forStatement(): Stmt {
    this.consume(TT.LEFT_PAREN, "Expect '(' after 'for'.");
    let initializer;
    if (this.match(TT.SEMICOLON)) {
      initializer = null;
    } else if (this.match(TT.VAR)) {
      initializer = this.varDeclaration();
    } else {
      initializer = this.expressionStatement();
    }
    let condition = null;
    if (!this.check(TT.SEMICOLON)) {
      condition = this.expression();
    }
    this.consume(TT.SEMICOLON, "Expect ';' after loop condition.");
    let increment = null;
    if (!this.check(TT.RIGHT_PAREN)) {
      increment = this.expression();
    }
    this.consume(TT.RIGHT_PAREN, "Expect ')' after for clauses.");
    let body = this.statement();
    if (increment !== null) {
      body = new stmt.Block([body, new stmt.Expression(increment)]);
    }
    if (condition == null) condition = new expr.Literal(true);
    body = new stmt.While(condition, body);
    if (initializer !== null) {
      body = new stmt.Block([initializer, body]);
    }
    return body;
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

  private returnStatement(): Stmt {
    const keyword = this.previous();
    let value = null;
    if (!this.check(TT.SEMICOLON)) {
      value = this.expression();
    }
    this.consume(TT.SEMICOLON, "Expect ';' after return value.");
    return new stmt.Return(keyword, value);
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

  private whileStatement() {
    this.consume(TT.LEFT_PAREN, "Expect '(' after 'while'.");
    const condition = this.expression();
    this.consume(TT.RIGHT_PAREN, "Expect ')' after condition.");
    const body = this.statement();
    return new stmt.While(condition, body);
  }

  private expressionStatement(): Stmt {
    const exp = this.expression();
    this.consume(TT.SEMICOLON, "Expect ';' after expression.");
    return new stmt.Expression(exp);
  }

  private function(kind: string): stmt.Function {
    const name = this.consume(TT.IDENTIFIER, `Expect ${kind} name.`);
    this.consume(TT.LEFT_PAREN, `Expect '(' after ${kind} name.`);
    const params: Token[] = [];
    if (!this.check(TT.RIGHT_PAREN)) {
      do {
        if (params.length >= 255) {
          this.error(this.peek(), "Can't have more than 255 parameters.");
        }
        params.push(this.consume(TT.IDENTIFIER, "Expect parameter name."));
      } while (this.match(TT.COMMA));
    }
    this.consume(TT.RIGHT_PAREN, "Expect ')' after parameters.");
    this.consume(TT.LEFT_BRACE, `Expect '{' before ${kind} body.`);
    const body = this.block();
    return new stmt.Function(name, params, body);
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
      } else if (exp instanceof expr.Get) {
        const get = exp as expr.Get;
        return new expr.Set(get.object, get.name, value);
      } else if (exp instanceof expr.GetDyn) {
        const get = exp as expr.GetDyn;
        return new expr.SetDyn(get.object, get.dot, get.name, value);
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
    return this.call();
  }

  private finishCall(callee: Expr): Expr {
    const args: Expr[] = [];
    if (!this.check(TT.RIGHT_PAREN)) {
      do {
        if (args.length >= 255) {
          this.error(this.peek(), "Can't have more than 255 arguments.");
        }
        args.push(this.expression());
      } while (this.match(TT.COMMA));
    }
    const paren = this.consume(TT.RIGHT_PAREN, "Expect ')' after arguments.");
    return new expr.Call(callee, paren, args);
  }

  private call(): Expr {
    let exp = this.primary();
    while (true) {
      if (this.match(TT.LEFT_PAREN)) {
        exp = this.finishCall(exp);
      } else if (this.match(TT.DOT)) {
        if (this.match(TT.LEFT_PAREN)) {
          const dot = this.previous();
          const name = this.expression();
          this.consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
          exp = new expr.GetDyn(exp, dot, name);
        } else {
          // Converts identifier-property accesses into dynamic
          // get, but dynamic get handler also returns nil
          // for not-found properties.
          // const name = this.consume(
          //   TT.IDENTIFIER,
          //   "Expect property name after '.'.",
          // );
          // const propName = new expr.Literal(name.lexeme);
          // exp = new expr.GetDyn(exp, name, propName);

          const name = this.consume(
            TT.IDENTIFIER,
            "Expect property name after '.'.",
          );
          exp = new expr.Get(exp, name);
        }
      } else {
        break;
      }
    }
    return exp;
  }

  private primary(): Expr {
    if (this.match(TT.FALSE)) return new expr.Literal(false);
    if (this.match(TT.TRUE)) return new expr.Literal(true);
    if (this.match(TT.NIL)) return new expr.Literal(null);

    if (this.match(TT.NUMBER, TT.STRING)) {
      return new expr.Literal(this.previous().literal);
    }

    if (this.match(TT.SUPER)) {
      const keyword = this.previous();
      this.consume(TT.DOT, "Expect '.' after 'super'.");
      const method = this.consume(
        TT.IDENTIFIER,
        "Expect superclass method name.",
      );
      return new expr.Super(keyword, method);
    }

    if (this.match(TT.THIS)) return new expr.This(this.previous());

    if (this.match(TT.IDENTIFIER)) {
      return new expr.Variable(this.previous());
    }

    if (this.match(TT.LEFT_PAREN)) {
      const exp = this.expression();
      this.consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
      return new expr.Grouping(exp);
    }

    throw this.error(this.peek(), "Expect expression.");
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

  constructor(enclosing?: Environment) {
    if (enclosing) {
      this.enclosing = enclosing;
    } else {
      this.enclosing = null;
    }
  }

  define(name: string, value: any) {
    this.values.set(name, value);
  }

  ancestor(distance: number): Environment {
    let environment: Environment = this;
    for (let i = 0; i < distance; i++) {
      environment = environment.enclosing as Environment;
    }
    return environment;
  }

  getAt(distance: number, name: string) {
    return this.ancestor(distance).values.get(name);
  }

  assignAt(distance: number, name: Token, value: any) {
    this.ancestor(distance).values.set(name.lexeme, value);
  }

  get(name: Token): any {
    if (this.values.has(name.lexeme)) {
      return this.values.get(name.lexeme);
    }
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

class Return extends Error {
  value: any;
  constructor(value: any) {
    super();
    this.value = value;
  }
}

abstract class LoxCallable {
  abstract arity(): number;
  abstract call(interpreter: Interpreter, args: any[]): any;
}

enum FunctionType {
  NONE,
  FUNCTION,
  INITIALIZER,
  METHOD,
}

class LoxFunction extends LoxCallable {
  private declaration: stmt.Function;
  private closure: Environment;
  private isInitializer: boolean;

  constructor(
    declaration: stmt.Function,
    closure: Environment,
    isInitializer: boolean,
  ) {
    super();
    this.declaration = declaration;
    this.closure = closure;
    this.isInitializer = isInitializer;
  }
  bind(instance: LoxInstance): LoxFunction {
    const environment = new Environment(this.closure);
    environment.define("this", instance);
    return new LoxFunction(this.declaration, environment, this.isInitializer);
  }
  call(interpreter: Interpreter, args: any[]): any {
    const environment = new Environment(this.closure);
    for (let i = 0; i < this.declaration.parameters.length; i++) {
      environment.define(this.declaration.parameters[i].lexeme, args[i]);
    }
    try {
      interpreter.executeBlock(this.declaration.body, environment);
    } catch (err) {
      if (err instanceof Return) {
        if (this.isInitializer) return this.closure.getAt(0, "this");
        return (err as Return).value;
      } else {
        throw err;
      }
    }
    if (this.isInitializer) {
      return this.closure.getAt(0, "this");
    }
    return null;
  }
  arity(): number {
    return this.declaration.parameters.length;
  }
  toString(): string {
    return `<fn ${this.declaration.name.lexeme}>`;
  }
}

enum ClassType {
  NONE,
  CLASS,
  SUBCLASS,
}

class LoxClass extends LoxCallable {
  name: string;
  private methods: Map<string, LoxFunction>;
  private superclass: LoxClass | null;
  constructor(
    name: string,
    superclass: LoxClass | null,
    methods: Map<string, LoxFunction>,
  ) {
    super();
    this.name = name;
    this.superclass = superclass;
    this.methods = methods;
  }
  findMethod(name: string): LoxFunction | null {
    if (this.methods.has(name)) {
      return this.methods.get(name) as LoxFunction;
    }
    if (null !== this.superclass) {
      return this.superclass.findMethod(name) as LoxFunction;
    }
    return null;
  }
  toString() {
    return this.name;
  }
  call(interpreter: Interpreter, args: any[]): any {
    const instance = new LoxInstance(this);
    const initializer = this.findMethod("init");
    if (null !== initializer) {
      initializer.bind(instance).call(interpreter, args);
    }
    return instance;
  }
  arity(): number {
    const initializer = this.findMethod("init");
    if (null === initializer) {
      return 0;
    } else {
      return initializer.arity();
    }
  }
}

class LoxInstance {
  private klass: LoxClass;
  private fields: Map<string, any> = new Map<string, any>();
  constructor(klass: LoxClass) {
    this.klass = klass;
  }
  toString() {
    return this.klass.name + " instance";
  }
  get(name: Token): any {
    if (this.fields.has(name.lexeme)) {
      return this.fields.get(name.lexeme);
    }
    const method = this.klass.findMethod(name.lexeme);
    if (null !== method) return method.bind(this);
    throw new RuntimeError(name, `Undefined property '${name.lexeme}'.`);
  }
  getDyn(name: any, where: Token): any {
    if (this.fields.has(name)) {
      return this.fields.get(name);
    }
    const method = this.klass.findMethod(name);
    if (null !== method) return method.bind(this);
    return null;
    // throw new RuntimeError(where, `Undefined property '${name}'.`);
  }
  set(name: Token, value: any) {
    this.fields.set(name.lexeme, value);
  }
  setDyn(name: any, value: any, where: Token): any {
    this.fields.set(name, value);
  }
}

class Resolver implements expr.Visitor<void>, stmt.Visitor<void> {
  private interpreter: Interpreter;
  private scopes: Map<string, boolean>[] = [];
  private currentFunction: FunctionType = FunctionType.NONE;
  private currentClass: ClassType = ClassType.NONE;

  constructor(interpreter: Interpreter) {
    this.interpreter = interpreter;
  }

  resolveStmts(statements: Stmt[]) {
    for (const statement of statements) {
      this.resolveStmt(statement);
    }
  }
  resolveStmt(stm: Stmt) {
    stm.accept(this);
  }
  resolveExpr(exp: Expr) {
    exp.accept(this);
  }
  resolveFunction(func: stmt.Function, typ: FunctionType) {
    const enclosingFunction = this.currentFunction;
    this.currentFunction = typ;
    this.beginScope();
    for (const param of func.parameters) {
      this.declare(param);
      this.define(param);
    }
    this.resolveStmts(func.body);
    this.endScope();
    this.currentFunction = enclosingFunction;
  }

  beginScope() {
    this.scopes.push(new Map<string, boolean>());
  }
  endScope() {
    this.scopes.pop();
  }
  declare(name: Token) {
    if (this.scopes.length < 1) return;
    const scope = this.scopes[this.scopes.length - 1];
    if (scope.has(name.lexeme)) {
      Lox.errorToken(name, "Already a variable with this name in this scope.");
    }
    scope.set(name.lexeme, false);
  }
  define(name: Token) {
    if (this.scopes.length < 1) return;
    const scope = this.scopes[this.scopes.length - 1];
    scope.set(name.lexeme, true);
  }
  resolveLocal(exp: Expr, name: Token) {
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      if (this.scopes[i].has(name.lexeme)) {
        this.interpreter.resolve(exp, this.scopes.length - 1 - i);
        return;
      }
    }
  }

  visitBlockStmt(stm: stmt.Block) {
    this.beginScope();
    this.resolveStmts(stm.statements);
    this.endScope();
  }
  visitClassStmt(stm: stmt.Class) {
    const enclosingClass = this.currentClass;
    this.currentClass = ClassType.CLASS;

    this.declare(stm.name);
    this.define(stm.name);

    if (
      null !== stm.superclass && stm.name.lexeme === stm.superclass.name.lexeme
    ) {
      Lox.errorToken(stm.superclass.name, "A class can't inherit from itself.");
    }
    if (null !== stm.superclass) {
      this.currentClass = ClassType.SUBCLASS;
      this.resolveExpr(stm.superclass);
    }
    if (null !== stm.superclass) {
      this.beginScope();
      this.scopes[this.scopes.length - 1].set("super", true);
    }

    this.beginScope();
    this.scopes[this.scopes.length - 1].set("this", true);
    for (const method of stm.methods) {
      let declaration = FunctionType.METHOD;
      if (method.name.lexeme === "init") {
        declaration = FunctionType.INITIALIZER;
      }
      this.resolveFunction(method, declaration);
    }
    this.endScope();

    if (null !== stm.superclass) this.endScope();

    this.currentClass = enclosingClass;
  }
  visitExpressionStmt(stm: stmt.Expression) {
    this.resolveExpr(stm.expression);
  }
  visitFunctionStmt(stm: stmt.Function) {
    this.declare(stm.name);
    this.define(stm.name);
    this.resolveFunction(stm, FunctionType.FUNCTION);
  }
  visitIfStmt(stm: stmt.If) {
    this.resolveExpr(stm.condition);
    this.resolveStmt(stm.thenBranch);
    if (stm.elseBranch !== null) this.resolveStmt(stm.elseBranch);
  }
  visitPrintStmt(stm: stmt.Print) {
    this.resolveExpr(stm.expression);
  }
  visitReturnStmt(stm: stmt.Return) {
    if (this.currentFunction === FunctionType.NONE) {
      Lox.errorToken(stm.keyword, "Can't return from top-level code.");
    }
    if (stm.value !== null) {
      if (this.currentFunction === FunctionType.INITIALIZER) {
        Lox.errorToken(
          stm.keyword,
          "Can't return a value from an initializer.",
        );
      }
      this.resolveExpr(stm.value);
    }
  }
  visitVarStmt(stm: stmt.Var) {
    this.declare(stm.name);
    if (stm.initializer !== null) {
      this.resolveExpr(stm.initializer);
    }
    this.define(stm.name);
  }
  visitWhileStmt(stm: stmt.While) {
    this.resolveExpr(stm.condition);
    this.resolveStmt(stm.body);
  }
  visitAssignExpr(exp: expr.Assign) {
    this.resolveExpr(exp.value);
    this.resolveLocal(exp, exp.name);
  }
  visitBinaryExpr(exp: expr.Binary) {
    this.resolveExpr(exp.left);
    this.resolveExpr(exp.right);
  }
  visitCallExpr(exp: expr.Call) {
    this.resolveExpr(exp.callee);
    for (const arg of exp.args) {
      this.resolveExpr(arg);
    }
  }
  visitGetExpr(exp: expr.Get) {
    this.resolveExpr(exp.object);
  }
  visitGetDynExpr(exp: expr.GetDyn) {
    this.resolveExpr(exp.object);
    this.resolveExpr(exp.name);
  }
  visitGroupingExpr(exp: expr.Grouping) {
    this.resolveExpr(exp.expression);
  }
  visitLiteralExpr(exp: expr.Literal) {}
  visitLogicalExpr(exp: expr.Logical) {
    this.resolveExpr(exp.left);
    this.resolveExpr(exp.right);
  }
  visitSetExpr(exp: expr.Set) {
    this.resolveExpr(exp.value);
    this.resolveExpr(exp.object);
  }
  visitSetDynExpr(exp: expr.SetDyn) {
    this.resolveExpr(exp.value);
    this.resolveExpr(exp.name);
    this.resolveExpr(exp.object);
  }
  visitSuperExpr(exp: expr.Super) {
    if (this.currentClass === ClassType.NONE) {
      Lox.errorToken(exp.keyword, "Can't use 'super' outside of a class.");
    } else if (this.currentClass !== ClassType.SUBCLASS) {
      Lox.errorToken(
        exp.keyword,
        "Can't use 'super' in a class with no superclass.",
      );
    }
    this.resolveLocal(exp, exp.keyword);
  }
  visitThisExpr(exp: expr.This) {
    if (this.currentClass === ClassType.NONE) {
      Lox.errorToken(exp.keyword, "Can't use 'this' outside of a class.");
      return;
    }
    this.resolveLocal(exp, exp.keyword);
  }
  visitUnaryExpr(exp: expr.Unary) {
    this.resolveExpr(exp.right);
  }
  visitVariableExpr(exp: expr.Variable) {
    if (
      !(this.scopes.length < 1) &&
      (this.scopes[this.scopes.length - 1]).get(exp.name.lexeme) === false
    ) {
      Lox.errorToken(
        exp.name,
        "Can't read local variable in its own initializer.",
      );
    }
    this.resolveLocal(exp, exp.name);
  }
}

class Interpreter implements expr.Visitor<any>, stmt.Visitor<void> {
  globals: Environment = new Environment();
  private environment: Environment = this.globals;
  private locals: Map<Expr, number> = new Map<Expr, number>();

  constructor() {
    class RtClock extends LoxCallable {
      arity(): number {
        return 0;
      }
      call(interpreter: Interpreter, args: any[]): any {
        return Date.now() / 1000;
      }
      toString(): string {
        return "<native fn>";
      }
    }
    class RtStr extends LoxCallable {
      arity(): number {
        return 1;
      }
      call(interpreter: Interpreter, args: any[]): any {
        return Interpreter.stringify(args[0]);
      }
    }
    class RtParseFloat extends LoxCallable {
      arity(): number {
        return 1;
      }
      call(interpreter: Interpreter, args: any[]): any {
        const v = Number.parseFloat(args[0]);
        if (isNaN(v)) return null;
        return v;
      }
    }
    class RtStrslice extends LoxCallable {
      arity(): number {
        return 3;
      }
      call(interpreter: Interpreter, args: any[]): any {
        const str = args[0];
        const start = args[1];
        const end = args[2];
        if (typeof str !== "string") {
          throw new RuntimeError({} as Token, "Invalid arg to substr");
        }
        return str.substring(start, end);
      }
    }
    class RtStrlen extends LoxCallable {
      arity(): number {
        return 1;
      }
      call(interpreter: Interpreter, args: any[]): any {
        const str = args[0];
        if (typeof str !== "string") {
          throw new RuntimeError({} as Token, "Invalid arg to strlen");
        }
        return str.length;
      }
    }
    class RtReadfile extends LoxCallable {
      arity(): number {
        return 1;
      }
      call(interpreter: Interpreter, args: any[]): any {
        try {
          const contents = Deno.readTextFileSync(args[0]);
          return contents;
        } catch (err) {
          throw new RuntimeError(
            {} as Token,
            `Failed to read '${args[0]}': ` + err.message,
          );
        }
      }
    }
    class RtProcessArgs extends LoxCallable {
      arity(): number {
        return 0;
      }
      call(interpreter: Interpreter, args: any[]): any {
        // Assume the lox.ts script was run like:
        // deno run --allow-read lox.ts lox.lox -- arg0 arg1 arg2
        // Skip past lox.lox filename and "--" separator.
        const runtimeArgs = Deno.args.slice(2);
        const containerClass = new LoxClass(
          "RtArgContainer",
          null,
          new Map<string, LoxFunction>(),
        );
        const container = new LoxInstance(containerClass);
        for (let i = 0; i < runtimeArgs.length; i++) {
          container.setDyn(
            i,
            runtimeArgs[i],
            new Token(TT.NUMBER, "" + i, i, 0),
          );
        }
        container.set(
          new Token(TT.STRING, "count", "count", 0),
          runtimeArgs.length,
        );
        return container;
      }
    }
    class RtProcessExit extends LoxCallable {
      arity(): number {
        return 1;
      }
      call(interpreter: Interpreter, args: any[]): any {
        const code = args[0];
        Deno.exit(code);
      }
    }
    class RtEprint extends LoxCallable {
      arity(): number {
        return 1;
      }
      call(interpreter: Interpreter, args: any[]): any {
        console.error(args[0]);
      }
    }

    this.globals.define("clock", new RtClock());
    this.globals.define("str", new RtStr());
    this.globals.define("strslice", new RtStrslice());
    this.globals.define("strlen", new RtStrlen());
    this.globals.define("readfile", new RtReadfile());
    this.globals.define("parse_float", new RtParseFloat());
    this.globals.define("process_args", new RtProcessArgs());
    this.globals.define("process_exit", new RtProcessExit());
    this.globals.define("eprint", new RtEprint());
  }

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

  resolve(exp: Expr, depth: number) {
    this.locals.set(exp, depth);
  }

  executeBlock(statements: Stmt[], environment: Environment) {
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

  visitClassStmt(stm: stmt.Class) {
    let superclass = null;
    if (null !== stm.superclass) {
      superclass = this.evaluate(stm.superclass);
      if (!(superclass instanceof LoxClass)) {
        throw new RuntimeError(
          stm.superclass.name,
          "Superclass must be a class.",
        );
      }
    }

    this.environment.define(stm.name.lexeme, null);

    if (null !== superclass) {
      this.environment = new Environment(this.environment);
      this.environment.define("super", superclass);
    }

    const methods = new Map<string, LoxFunction>();
    for (const method of stm.methods) {
      const func = new LoxFunction(
        method,
        this.environment,
        method.name.lexeme === "init",
      );
      methods.set(method.name.lexeme, func);
    }
    const klass = new LoxClass(
      stm.name.lexeme,
      superclass as LoxClass,
      methods,
    );

    if (null !== superclass) {
      this.environment = this.environment.enclosing as Environment;
    }

    this.environment.assign(stm.name, klass);
  }

  visitExpressionStmt(stm: stmt.Expression) {
    this.evaluate(stm.expression);
  }

  visitFunctionStmt(stm: stmt.Function) {
    const func = new LoxFunction(stm, this.environment, false);
    this.environment.define(stm.name.lexeme, func);
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
    // Rely on console.log for now since
    // stdout.write returns a Promise that
    // a runtime error could interrupt.
    // console.log(this.stringify(value));
    Deno.stdout.writeSync(ENC.encode(Interpreter.stringify(value) + "\n"));
  }

  visitReturnStmt(stm: stmt.Return) {
    let value = null;
    if (stm.value !== null) value = this.evaluate(stm.value);
    throw new Return(value);
  }

  visitVarStmt(stm: stmt.Var) {
    let value = null;
    if (stm.initializer != null) {
      value = this.evaluate(stm.initializer);
    }
    this.environment.define(stm.name.lexeme, value);
    return null;
  }

  visitWhileStmt(stm: stmt.While) {
    while (this.isTruthy(this.evaluate(stm.condition))) {
      this.execute(stm.body);
    }
  }

  visitAssignExpr(exp: expr.Assign): any {
    const value = this.evaluate(exp.value);
    const distance = this.locals.get(exp);
    if (distance !== null && distance !== undefined) {
      this.environment.assignAt(distance, exp.name, value);
    } else {
      this.globals.assign(exp.name, value);
    }
    return value;
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

  visitCallExpr(exp: expr.Call): any {
    const callee = this.evaluate(exp.callee);
    const args: any[] = [];
    for (const arg of exp.args) {
      args.push(this.evaluate(arg));
    }
    if (!(callee instanceof LoxCallable)) {
      throw new RuntimeError(exp.paren, "Can only call functions and classes.");
    }
    const func = callee as LoxCallable;
    if (args.length !== func.arity()) {
      throw new RuntimeError(
        exp.paren,
        `Expected ${func.arity()} arguments but got ${args.length}.`,
      );
    }
    return func.call(this, args);
  }

  visitGetExpr(exp: expr.Get): any {
    const object = this.evaluate(exp.object);
    if (object instanceof LoxInstance) {
      return (object as LoxInstance).get(exp.name);
    }
    throw new RuntimeError(exp.name, "Only instances have properties.");
  }

  visitGetDynExpr(exp: expr.GetDyn): any {
    const object = this.evaluate(exp.object);
    if (object instanceof LoxInstance) {
      const name = this.evaluate(exp.name);
      return (object as LoxInstance).getDyn(name, exp.dot);
    }
    throw new RuntimeError(exp.dot, "Only instances have properties.");
  }

  visitGroupingExpr(exp: expr.Grouping): any {
    return this.evaluate(exp.expression);
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

  visitSetExpr(exp: expr.Set): any {
    const object = this.evaluate(exp.object);
    if (!(object instanceof LoxInstance)) {
      throw new RuntimeError(exp.name, "Only instances have fields.");
    }
    const value = this.evaluate(exp.value);
    (object as LoxInstance).set(exp.name, value);
    return value;
  }

  visitSetDynExpr(exp: expr.SetDyn): any {
    const object = this.evaluate(exp.object);
    if (!(object instanceof LoxInstance)) {
      throw new RuntimeError(exp.dot, "Only instances have fields.");
    }
    const name = this.evaluate(exp.name);
    const value = this.evaluate(exp.value);
    (object as LoxInstance).setDyn(name, value, exp.dot);
  }

  visitSuperExpr(exp: expr.Super): any {
    const distance = this.locals.get(exp) as number;
    const superclass = this.environment.getAt(distance, "super") as LoxClass;
    const object = this.environment.getAt(distance - 1, "this") as LoxInstance;
    const method = superclass.findMethod(exp.method.lexeme);
    if (null === method) {
      throw new RuntimeError(
        exp.method,
        `Undefined property '${exp.method.lexeme}'.`,
      );
    }
    return method.bind(object);
  }

  visitThisExpr(exp: expr.This): any {
    return this.lookupVariable(exp.keyword, exp);
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
    return this.lookupVariable(exp.name, exp);
  }

  private lookupVariable(name: Token, exp: Expr): any {
    const distance = this.locals.get(exp);
    if (distance !== null && distance !== undefined) {
      return this.environment.getAt(distance, name.lexeme);
    } else {
      return this.globals.get(name);
    }
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

  static stringify(object: any) {
    if (null === object) return "nil";
    if (typeof object === "number") {
      if (object === 0) {
        const bytes = Interpreter.doubleToByteArray(object);
        // Not really sure why this is expected...
        if (bytes[0] === -128) return "-0";
      }
      return JSON.stringify(object);
    }
    if (typeof object === "string") {
      return object;
    }
    if (object.toString) {
      return object.toString();
    }
    return JSON.stringify(object);
  }

  private static doubleToByteArray(num: number) {
    const buffer = new ArrayBuffer(8); // JS numbers are 8 bytes long, or 64 bits
    const longNum = new Float64Array(buffer); // so equivalent to Float64
    longNum[0] = num;
    return Array.from(new Int8Array(buffer)).reverse(); // reverse to get little endian
  }
}

class Scanner {
  source: string;
  tokens: Token[] = [];
  private start: number = 0;
  private current: number = 0;
  private line: number = 1;

  static keywords: Map<string, TokenType> = Scanner.initKeywords();

  private static initKeywords(): Map<string, TokenType> {
    const keywords = new Map<string, TokenType>();
    keywords.set("and", TT.AND);
    keywords.set("class", TT.CLASS);
    keywords.set("else", TT.ELSE);
    keywords.set("false", TT.FALSE);
    keywords.set("for", TT.FOR);
    keywords.set("fun", TT.FUN);
    keywords.set("if", TT.IF);
    keywords.set("nil", TT.NIL);
    keywords.set("or", TT.OR);
    keywords.set("print", TT.PRINT);
    keywords.set("return", TT.RETURN);
    keywords.set("super", TT.SUPER);
    keywords.set("this", TT.THIS);
    keywords.set("true", TT.TRUE);
    keywords.set("var", TT.VAR);
    keywords.set("while", TT.WHILE);
    return keywords;
  }

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
    let typ: TokenType | null = Scanner.keywords.get(text) || null;
    if (typ === null) typ = TT.IDENTIFIER;
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

  private unescape(source: string, transforms: string[][]) {
    const len = source.length;
    let start = 0;
    let end = 0;
    let result = "";

    for (var i = 0; i < len; i = i + 1) {
      let replaced = false;
      for (var j = 0; j < transforms.length; j++) {
        const [from, to] = transforms[j];
        const potential = source.substring(i, i + from.length);
        if (potential === from) {
          result = result + source.substring(start, end) + to;
          start = i + from.length;
          end = start;
          i = i + from.length - 1;
          replaced = true;
          break;
        }
      }
      if (!replaced) end = end + 1;
    }

    return result + source.substring(start, len);
  }

  private string() {
    while (this.peek() != '"' && !this.isAtEnd()) {
      if (this.peek() == "\n") this.line++;
      if (this.peek() == "\\") this.advance();
      this.advance();
    }

    if (this.isAtEnd()) {
      Lox.error(this.line, "Unterminated string.");
      return;
    }

    // The closing ".
    this.advance();

    // Trim the surrounding quotes.
    let value = this.source.substring(this.start + 1, this.current - 1);
    value = this.unescape(value, [
      ["\\0", "\0"],
      ["\\r", "\r"],
      ["\\\\", "\\"],
      ["\\n", "\n"],
      ["\\t", "\t"],
      ["\\\"", "\""],
    ]);
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
  visitAssignExpr(exp: expr.Assign) {
    Deno.stdout.write(ENC.encode(`(assign ${exp.name.lexeme} `));
    exp.value.accept(this);
    Deno.stdout.write(ENC.encode(`)`));
  }
  visitCallExpr(exp: expr.Call) {
  }
  visitLogicalExpr(exp: expr.Logical) {
  }
  visitVariableExpr(exp: expr.Variable) {
    Deno.stdout.write(ENC.encode(`(var ${exp.name.lexeme})`));
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
  const parser = new Parser(tokens);
  const statements = parser.parse();

  if (Lox.hadError) return;

  const resolver = new Resolver(Lox.interpreter);
  resolver.resolveStmts(statements);

  if (Lox.hadError) return;

  Lox.interpreter.interpret(statements);
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
    if (args[1] === "--") {
      await runFile(Deno.args[0]);
    } else {
      console.log(`Usage: deno run --allow-read lox.ts [script]`);
      Deno.exit(64);
    }
  } else if (args.length == 1) {
    await runFile(Deno.args[0]);
  } else {
    await runPrompt();
  }
}
