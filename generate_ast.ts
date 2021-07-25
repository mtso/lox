// Usage: deno run generate_ast.ts > ast.ts

const defineVisitor = (baseName: string, types: string[]) => {
  console.log(`  export type Visitor<T> = {`);
  for (const typ of types) {
    const className = typ.split("=")[0].trim();
    console.log(`    visit${className}${baseName}: (exp: ${className}) => T;`);
  }
  console.log(`  };`);
};

const defineType = (baseName: string, className: string, fields: string[]) => {
  console.log(`  export class ${className} extends ${baseName} {`);
  for (const field of fields) {
    console.log(`    ${field};`);
  }
  console.log(`\n    constructor(${fields.join(", ")}) {
      super();`);
  for (const field of fields) {
    const name = field.split(": ")[0];
    console.log(`      this.${name} = ${name};`);
  }
  console.log(`    }`);
  console.log(`    accept<T>(visitor: Visitor<T>): T {
      return visitor.visit${className}${baseName}(this);
    }`);
  console.log(`  }`);
};

const defineAst = (baseName: string, types: string[]) => {
  console.log(`  export abstract class ${baseName} {
    abstract accept<T>(visitor: Visitor<T>): T;
  }`);

  defineVisitor(baseName, types);

  for (const typ of types) {
    const className = typ.split("=")[0].trim();
    const fields = typ.split("=")[1].trim().split(", ");
    defineType(baseName, className, fields);
  }
};

{
  console.log(`/* THIS FILE WAS GENERATED */\n`);
  console.log(`import { Token } from "./lox.ts";`);
  console.log(`\nexport module expr {`);
  defineAst("Expr", [
    "Assign   = name: Token, value: Expr",
    "Binary   = left: Expr, operator: Token, right: Expr",
    "Grouping = expression: Expr",
    "Literal  = value: any",
    "Logical  = left: Expr, operator: Token, right: Expr",
    "Unary    = operator: Token, right: Expr",
    "Variable = name: Token",
  ]);
  console.log(`}`);
  console.log(`\nexport module stmt {
  type Expr = expr.Expr;`);
  defineAst("Stmt", [
    "Block      = statements: Stmt[]",
    "Expression = expression: Expr",
    "If         = condition: Expr, thenBranch: Stmt, elseBranch: Stmt | null",
    "Print      = expression: Expr",
    "Var        = name: Token, initializer: Expr | null",
  ]);
  console.log(`}`);
}
