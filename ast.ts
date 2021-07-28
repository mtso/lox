/* THIS FILE WAS GENERATED */

import { Token } from "./lox.ts";

export module expr {
  export abstract class Expr {
    abstract accept<T>(visitor: Visitor<T>): T;
  }
  export type Visitor<T> = {
    visitAssignExpr: (exp: Assign) => T;
    visitBinaryExpr: (exp: Binary) => T;
    visitCallExpr: (exp: Call) => T;
    visitGetDynExpr: (exp: GetDyn) => T;
    visitGetExpr: (exp: Get) => T;
    visitSetExpr: (exp: Set) => T;
    visitSetDynExpr: (exp: SetDyn) => T;
    visitSuperExpr: (exp: Super) => T;
    visitThisExpr: (exp: This) => T;
    visitGroupingExpr: (exp: Grouping) => T;
    visitLambdaExpr: (exp: Lambda) => T;
    visitLiteralExpr: (exp: Literal) => T;
    visitLogicalExpr: (exp: Logical) => T;
    visitUnaryExpr: (exp: Unary) => T;
    visitVariableExpr: (exp: Variable) => T;
  };
  export class Assign extends Expr {
    name: Token;
    value: Expr;

    constructor(name: Token, value: Expr) {
      super();
      this.name = name;
      this.value = value;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitAssignExpr(this);
    }
  }
  export class Binary extends Expr {
    left: Expr;
    operator: Token;
    right: Expr;

    constructor(left: Expr, operator: Token, right: Expr) {
      super();
      this.left = left;
      this.operator = operator;
      this.right = right;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitBinaryExpr(this);
    }
  }
  export class Call extends Expr {
    callee: Expr;
    paren: Token;
    args: Expr[];

    constructor(callee: Expr, paren: Token, args: Expr[]) {
      super();
      this.callee = callee;
      this.paren = paren;
      this.args = args;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitCallExpr(this);
    }
  }
  export class GetDyn extends Expr {
    object: Expr;
    dot: Token;
    name: Expr;

    constructor(object: Expr, dot: Token, name: Expr) {
      super();
      this.object = object;
      this.dot = dot;
      this.name = name;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitGetDynExpr(this);
    }
  }
  export class Get extends Expr {
    object: Expr;
    name: Token;

    constructor(object: Expr, name: Token) {
      super();
      this.object = object;
      this.name = name;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitGetExpr(this);
    }
  }
  export class Set extends Expr {
    object: Expr;
    name: Token;
    value: Expr;

    constructor(object: Expr, name: Token, value: Expr) {
      super();
      this.object = object;
      this.name = name;
      this.value = value;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitSetExpr(this);
    }
  }
  export class SetDyn extends Expr {
    object: Expr;
    dot: Token;
    name: Expr;
    value: Expr;

    constructor(object: Expr, dot: Token, name: Expr, value: Expr) {
      super();
      this.object = object;
      this.dot = dot;
      this.name = name;
      this.value = value;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitSetDynExpr(this);
    }
  }
  export class Super extends Expr {
    keyword: Token;
    method: Token;

    constructor(keyword: Token, method: Token) {
      super();
      this.keyword = keyword;
      this.method = method;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitSuperExpr(this);
    }
  }
  export class This extends Expr {
    keyword: Token;

    constructor(keyword: Token) {
      super();
      this.keyword = keyword;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitThisExpr(this);
    }
  }
  export class Grouping extends Expr {
    expression: Expr;

    constructor(expression: Expr) {
      super();
      this.expression = expression;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitGroupingExpr(this);
    }
  }
  export class Lambda extends Expr {
    name: Token | null;
    parameters: Token[];
    body: stmt.Stmt[];

    constructor(name: Token | null, parameters: Token[], body: stmt.Stmt[]) {
      super();
      this.name = name;
      this.parameters = parameters;
      this.body = body;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitLambdaExpr(this);
    }
  }
  export class Literal extends Expr {
    value: any;

    constructor(value: any) {
      super();
      this.value = value;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitLiteralExpr(this);
    }
  }
  export class Logical extends Expr {
    left: Expr;
    operator: Token;
    right: Expr;

    constructor(left: Expr, operator: Token, right: Expr) {
      super();
      this.left = left;
      this.operator = operator;
      this.right = right;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitLogicalExpr(this);
    }
  }
  export class Unary extends Expr {
    operator: Token;
    right: Expr;

    constructor(operator: Token, right: Expr) {
      super();
      this.operator = operator;
      this.right = right;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitUnaryExpr(this);
    }
  }
  export class Variable extends Expr {
    name: Token;

    constructor(name: Token) {
      super();
      this.name = name;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitVariableExpr(this);
    }
  }
}

export module stmt {
  type Expr = expr.Expr;
  export abstract class Stmt {
    abstract accept<T>(visitor: Visitor<T>): T;
  }
  export type Visitor<T> = {
    visitBlockStmt: (exp: Block) => T;
    visitClassStmt: (exp: Class) => T;
    visitExpressionStmt: (exp: Expression) => T;
    visitFunctionStmt: (exp: Function) => T;
    visitIfStmt: (exp: If) => T;
    visitPrintStmt: (exp: Print) => T;
    visitReturnStmt: (exp: Return) => T;
    visitVarStmt: (exp: Var) => T;
    visitWhileStmt: (exp: While) => T;
  };
  export class Block extends Stmt {
    statements: Stmt[];

    constructor(statements: Stmt[]) {
      super();
      this.statements = statements;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitBlockStmt(this);
    }
  }
  export class Class extends Stmt {
    name: Token;
    superclass: expr.Variable | null;
    methods: Function[];

    constructor(
      name: Token,
      superclass: expr.Variable | null,
      methods: Function[],
    ) {
      super();
      this.name = name;
      this.superclass = superclass;
      this.methods = methods;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitClassStmt(this);
    }
  }
  export class Expression extends Stmt {
    expression: Expr;

    constructor(expression: Expr) {
      super();
      this.expression = expression;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitExpressionStmt(this);
    }
  }
  export class Function extends Stmt {
    name: Token;
    parameters: Token[];
    body: Stmt[];

    constructor(name: Token, parameters: Token[], body: Stmt[]) {
      super();
      this.name = name;
      this.parameters = parameters;
      this.body = body;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitFunctionStmt(this);
    }
  }
  export class If extends Stmt {
    condition: Expr;
    thenBranch: Stmt;
    elseBranch: Stmt | null;

    constructor(condition: Expr, thenBranch: Stmt, elseBranch: Stmt | null) {
      super();
      this.condition = condition;
      this.thenBranch = thenBranch;
      this.elseBranch = elseBranch;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitIfStmt(this);
    }
  }
  export class Print extends Stmt {
    expression: Expr;

    constructor(expression: Expr) {
      super();
      this.expression = expression;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitPrintStmt(this);
    }
  }
  export class Return extends Stmt {
    keyword: Token;
    value: Expr | null;

    constructor(keyword: Token, value: Expr | null) {
      super();
      this.keyword = keyword;
      this.value = value;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitReturnStmt(this);
    }
  }
  export class Var extends Stmt {
    name: Token;
    initializer: Expr | null;

    constructor(name: Token, initializer: Expr | null) {
      super();
      this.name = name;
      this.initializer = initializer;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitVarStmt(this);
    }
  }
  export class While extends Stmt {
    condition: Expr;
    body: Stmt;

    constructor(condition: Expr, body: Stmt) {
      super();
      this.condition = condition;
      this.body = body;
    }
    accept<T>(visitor: Visitor<T>): T {
      return visitor.visitWhileStmt(this);
    }
  }
}
