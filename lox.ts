import { readLines } from "https://deno.land/std@0.102.0/io/bufio.ts";
import { expr } from "./ast.ts";

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

class Scanner {
  source: string;
  tokens: Token[] = [];
  start: number = 0;
  current: number = 0;
  line: number = 1;
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
          err.error(this.line, "Unexpected character.");
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
      err.error(this.line, "Unterminated string.");
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

module err {
  export let hadError: boolean = false;

  export const report = (line: number, where: string, message: string) => {
    console.error(`[line ${line}] Error${where}: ${message}`);
    hadError = true;
  };

  export const error = (line: number, message: string) => {
    report(line, "", message);
  };
}

class Printer implements expr.Visitor<void> {
  visitLiteral(exp: expr.Literal) {
    console.log(`<literal ${exp.value}>`);
  }
  visitBinary(exp: expr.Binary) {
    exp.left.accept(this);
    console.log(exp.operator.toString());
    exp.right.accept(this);
  }
  visitGrouping(exp: expr.Grouping) {
    exp.expression.accept(this);
  }
  visitUnary(exp: expr.Unary) {
    console.log(exp.operator.toString());
    exp.right.accept(this);
  }
}

const ENC = new TextEncoder();

function run(source: string) {
  const scanner = new Scanner(source);
  const tokens = scanner.scanTokens();
  console.log(tokens.map((t) => t.toString()));
}

async function runFile(file: string) {
  console.log(file);

  const contents = await Deno.readTextFile(file);
  run(contents);
}

async function runPrompt() {
  Deno.stdout.write(ENC.encode("> "));
  for await (const line of readLines(Deno.stdin)) {
    console.log(line);

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

  const p = new Printer();
  const l = new expr.Literal(5);
  l.accept(p);
}
