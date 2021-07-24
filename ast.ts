/* THIS FILE WAS GENERATED */

import { Token } from "./lox.ts";

export module expr {
  export abstract class Expr {
    abstract accept<T>(visitor: Visitor<T>): T;
  }
  export type Visitor<T> = {
    visitBinaryExpr: (exp: Binary) => T;
    visitGroupingExpr: (exp: Grouping) => T;
    visitLiteralExpr: (exp: Literal) => T;
    visitUnaryExpr: (exp: Unary) => T;
  };
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
}
