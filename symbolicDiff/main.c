#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>

typedef enum { PLUS, MINUS, SLASH, STAR, POW, LEFT_PAREN, RIGHT_PAREN, NUMBER, VAR, SIN, COS, TAN, LN, EXP } TokenType;
typedef enum {LITERAL, EXPR} ValType;

typedef struct {
  TokenType type;
  union { double number; } value;
} Token;

typedef struct {
  Token* tokens;
  size_t size;
} TokensList;

typedef struct {
  ValType valType;
  TokenType operator;
  void* op1;
  void* op2;
  void* interpretThyself;
} Expr;

typedef struct {
  ValType valType;
  TokenType type;
  union {
    double number;
  } value;
  void* interpretThyself;
} Literal;

void* simplify(void* expr);
double operate(double a, double b, TokenType op);


char* lisptify(void* expr);

void* dispatch(void* exprOrLiteral);
void printAST(void* exprOrLiteral);
void printTokens(TokensList tokens);
void* parse(TokensList t, int* idx, int sz);
void* expr(TokensList t, int* idx, int sz);
void* operand(TokensList t, int* idx,int sz);
bool isDigit(char c);

const char* diff (const char* expr);

void* derivNum();
void* derivVar();
void* derivAdd(Expr* expr);
void* derivSub(Expr* expr);
void* derivMult(Expr* expr);
void* derivQuot(Expr* expr);
void* derivPow(Expr* expr);

void* derivCos(Expr* expr);
void* derivSin(Expr* expr);
void* derivTan(Expr* expr);

void* derivLn(Expr* expr);
void* derivExp(Expr* expr);


char* lisptify(void* exprOrLiteral) {
  if (exprOrLiteral == NULL) return strdup("");

  ValType type = *((ValType*) exprOrLiteral);
  char* res = NULL;

  if (type == EXPR) {
    Expr* expr = (Expr*) exprOrLiteral;
    TokenType operator = *(((TokenType*) exprOrLiteral) + 1);

    asprintf(&res, "(");

    switch (operator) {
      case PLUS:
        asprintf(&res, "%s+ ", res);
        break;
      case MINUS:
        asprintf(&res, "%s- ", res);
        break;
      case STAR:
        asprintf(&res, "%s* ", res);
        break;
      case SLASH:
        asprintf(&res, "%s/ ", res);
        break;
      case POW:
        asprintf(&res, "%s^ ", res);
        break;
      case SIN:
        asprintf(&res, "%ssin ", res);
        break;
      case COS:
        asprintf(&res, "%scos ", res);
        break;
      case TAN:
        asprintf(&res, "%stan ", res);
        break;
      case EXP:
        asprintf(&res, "%sexp ", res);
        break;
      case LN:
        asprintf(&res, "%sln ", res);
        break;
      default:
        asprintf(&res, "%s? ", res);
        break;
    }

    asprintf(&res, "%s%s", res, lisptify(expr->op1));
    if (expr->op2) asprintf(&res, "%s %s", res, lisptify(expr->op2));

    asprintf(&res, "%s)", res);

    return res;
  }

  if (type == LITERAL) {
    Literal* literal = (Literal*) exprOrLiteral;
    TokenType varOrNum = *(((TokenType*) exprOrLiteral) + 1);

    if (varOrNum == NUMBER) {
      double num = literal->value.number;
      if(num == (int) num) {
        asprintf(&res, "%.0f", literal->value.number);
      } else {
        asprintf(&res, "%.1f", literal->value.number);
      }
    } else if (varOrNum == VAR) {
      asprintf(&res, "x");
    }

    return res;
  }

  return strdup("");
}

double operate(double a, double b, TokenType op) {
    switch(op) {
        case PLUS:
            return a + b;
        case MINUS:
            return a - b;
        case STAR:
            return a * b;
        case SLASH:
            return a / b;
        case POW:
            return pow(a, b);
    }
}

void* simplify(void* exprOrLiteral) {
    if (exprOrLiteral == NULL) return NULL;

    ValType type = *((ValType*) exprOrLiteral);

    if (type == EXPR) {
        Expr* form = (Expr*) exprOrLiteral;

        form->op1 = simplify(form->op1);
        form->op2 = simplify(form->op2);

        ValType o1t = *((ValType*) form->op1);
        ValType o2t = form->op2 ? *((ValType*) form->op2) : -10000;

        if (o1t == LITERAL && o2t == LITERAL) {
            Literal* a = (Literal*)form->op1;
            Literal* b = (Literal*)form->op2;
            TokenType at = a->type;
            TokenType bt = b->type;

            // Handle addition
            if (form->operator == PLUS) {
                if (at == NUMBER && bt == NUMBER) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = a->value.number + b->value.number;
                    return res;
                }
                if (at == NUMBER && a->value.number == 0) return b; // 0 + b = b
                if (bt == NUMBER && b->value.number == 0) return a; // a + 0 = a
            }


            // Handle subtraction
            if (form->operator == MINUS) {
                if (at == NUMBER && bt == NUMBER) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = a->value.number - b->value.number;
                    return res;
                }
                if (bt == NUMBER && b->value.number == 0) return a; // a - 0 = a
                if (at == NUMBER && a->value.number == b->value.number) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = 0; // a - a = 0
                    return res;
                }

                // 0 - a = -a
                 if (at == NUMBER && a->value.number == 0) {
                   b->value.number *= -1;
                   return b;
                }

              //return exprOrLiteral;
            }

            // Handle multiplication
            if (form->operator == STAR) {
                if (at == NUMBER && bt == NUMBER) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = a->value.number * b->value.number;
                    return res;
                }
                if ((at == NUMBER && a->value.number == 0) || (bt == NUMBER && b->value.number == 0)) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = 0; // 0 * anything = 0
                    return res;
                }
                if (at == NUMBER && a->value.number == 1) return b; // 1 * b = b
                if (bt == NUMBER && b->value.number == 1) return a; // a * 1 = a

                //return exprOrLiteral;
            }

            // Handle division
            if (form->operator == SLASH) {
                if (bt == NUMBER && b->value.number == 0) {
                    // Division by zero handling
                    return NULL; // or some error handling
                }
                if (at == NUMBER && bt == NUMBER) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = a->value.number / b->value.number;
                    return res;
                }
                if (bt == NUMBER && b->value.number == 1) return a; // a / 1 = a
                if (at == NUMBER && a->value.number == 0) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = 0; // 0 / anything = 0
                    return res;
                }
              //return exprOrLiteral;
            }

            // Handle exponentiation
            if (form->operator == POW) {
                if(bt == NUMBER && b->value.number == 1) {
                  return a;
                }
                if (at == NUMBER && bt == NUMBER) {
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = pow(a->value.number, b->value.number);
                    return res;
                }
                if (bt == NUMBER && b->value.number == 0) {
                    // Any number to the power of 0 is 1
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = 1;
                    return res;
                }
                if (at == NUMBER && a->value.number == 0 && b->value.number > 0) {
                    // 0 to any positive power is 0
                    Literal* res = malloc(sizeof(Literal));
                    res->valType = LITERAL;
                    res->type = NUMBER;
                    res->value.number = 0;
                    return res;
                }
                if (at == NUMBER && a->value.number == 1) {
                    return a; // 1 to any power is 1
                }

                  //return exprOrLiteral;

            }
        }


        if(o1t == LITERAL && o2t != LITERAL) {

            Literal* a = (Literal*)form->op1;
            Expr* b = (Expr*)form->op2;
          if(form->operator == PLUS) {
            if(a->value.number == 0) {
              return simplify(b);
            }
          }

          if(form->operator == STAR) {
            if(a->value.number == 0) {
              Literal* res = malloc(sizeof(Literal));
              res->valType = LITERAL;
              res->type = NUMBER;
              res->value.number = 0;
              return res;
            }

            if(a->value.number == 1) {
              return simplify(b);
            }
          }

        }


        if(o2t == LITERAL && o1t != LITERAL) {

                      Expr* a = (Expr*)form->op1;
            Literal* b = (Literal*)form->op2;
          if(form->operator == PLUS) {
            if(b->value.number == 0) {
              return simplify(a);
            }
          }

          if(form->operator == STAR) {
            if(b->value.number == 0) {
              Literal* res = malloc(sizeof(Literal));
              res->valType = LITERAL;
              res->type = NUMBER;
              res->value.number = 0;
              return res;
            }

            if(b->value.number == 1) {
              return simplify(a);
            }
          }

        }


      return form;
    }

    if (type == LITERAL) {
        return exprOrLiteral;
    }

    return exprOrLiteral;
}


void* dispatch(void* exprOrLiteral) {
  if(exprOrLiteral == NULL) return NULL;
  ValType type = *((ValType*) exprOrLiteral);

  if(type == EXPR) {
    Expr* expr = (Expr*) exprOrLiteral;
    return ((void* (*)(Expr*))expr->interpretThyself)((void*) expr);
  }

  if(type == LITERAL) {
    Literal* literal = (Literal*) exprOrLiteral;
    return ((void* (*)(void))literal->interpretThyself)();
  }

  return NULL;
}

void* derivNum() {
  Literal* literal = malloc(sizeof(literal));
  literal->valType = LITERAL;
  literal->type = NUMBER;
  literal->value.number = 0;

  return (void*) literal;
}

void* derivVar() {
  Literal* literal = malloc(sizeof(literal));
  literal->valType = LITERAL;
  literal->type = NUMBER;
  literal->value.number = 1;

  return (void*) literal;
}


void* derivAdd(Expr* expr) {
  Expr* newExpr = malloc(sizeof(Expr));
  newExpr->valType = EXPR;
  newExpr->operator = PLUS;

  newExpr->op1 = dispatch(expr->op1);//->interpretThyself();
  newExpr->op2 = dispatch(expr->op2);//interpretThyself();

  return (void*) newExpr;
}

void* derivSub(Expr* expr) {
  Expr* newExpr = malloc(sizeof(Expr));
  newExpr->valType = EXPR;
  newExpr->operator = MINUS;

  newExpr->op1 = dispatch(expr->op1);//->interpretThyself();
  newExpr->op2 = dispatch(expr->op2);//interpretThyself();

  return (void*) newExpr;
}

void* derivMult(Expr* expr) {
  // u'v
  Expr* u = malloc(sizeof(Expr));
  u->valType = EXPR;
  u->operator = STAR;

  u->op1 = dispatch(expr->op1);
  u->op2 = expr->op2;

  // v'u
  Expr* v = malloc(sizeof(Expr));
  v->valType = EXPR;
  v->operator = STAR;

  v->op1 = expr->op1;
  v->op2 = dispatch(expr->op2);

  // u'v + v'u
  Expr* uv = malloc(sizeof(Expr));
  uv->valType = EXPR;
  uv->operator = PLUS;

  uv->op1 = u;
  uv->op2 = v;

  return (void*) uv;
}

void* derivQuot(Expr* expr) {
  // u'v
  Expr* u = malloc(sizeof(Expr));
  u->valType = EXPR;
  u->operator = STAR;

  u->op1 = dispatch(expr->op1);
  u->op2 = expr->op2;

  // v'u
  Expr* v = malloc(sizeof(Expr));
  v->valType = EXPR;
  v->operator = STAR;

  v->op1 = expr->op1;
  v->op2 = dispatch(expr->op2);

  // u'v - v'u
  Expr* uv = malloc(sizeof(Expr));
  uv->valType = EXPR;
  uv->operator = MINUS;

  uv->op1 = u;
  uv->op2 = v;

  // v^2
  Expr* vv = malloc(sizeof(Expr));
  vv->valType = EXPR;
  vv->operator = POW;

  vv->op1 = expr->op2;

  Literal* pow2 = malloc(sizeof(Literal));
  pow2->valType = LITERAL;
  pow2->type = NUMBER;
  pow2->value.number = 2;

  vv->op2 = pow2;



  // Combine ( u'v - v'u ) / v^2

  Expr* quot = malloc(sizeof(Expr));
  quot->valType = EXPR;
  quot->operator = SLASH;

  quot->op1 = uv;
  quot->op2 = vv;

  return (void*) quot;
}

void* derivPow(Expr* expr) {
  // u'v
  Expr* u = malloc(sizeof(Expr));
  u->valType = EXPR;
  u->operator = STAR;

  // coefficient
  Literal* unary = malloc(sizeof(Literal));
  unary->valType = LITERAL;
  unary->type = NUMBER;
  unary->value.number = ((Literal*) expr->op2)->value.number;
  u->op1 = unary;

  // subtract 1 from the original expression power
  ((Literal*)expr->op2)->value.number -= 1;
  u->op2 = expr;
  //memcpy(u->op2, expr, sizeof(expr));

  return (void*) u;
}

void* derivSin(Expr* expr) {
  Expr* cos = malloc(sizeof(Expr));

  cos->valType = EXPR;
  cos->operator = COS;

  cos->op1 = expr->op1;

  Expr* final = malloc(sizeof(Expr));
  final->valType = EXPR;
  final->operator = STAR;

  final->op2 = cos;
  final->op1 = dispatch(expr->op1);

  return (void*) final;
}

void* derivCos(Expr* expr) {
  Expr* sin = malloc(sizeof(Expr));

  sin->valType = EXPR;
  sin->operator = SIN;

  sin->op1 = expr->op1;

  Expr* neg1 = malloc(sizeof(Expr));
  neg1->valType = EXPR;
  neg1->operator = STAR;

  neg1->op1 = sin;
  Literal* n1 = malloc(sizeof(Literal));
  n1->valType = LITERAL;
  n1->type = NUMBER;
  n1->value.number = -1;

  neg1->op1 = n1;
  neg1->op2 = sin;

  Expr* final = malloc(sizeof(Expr));
  final->valType = EXPR;
  final->operator = STAR;

  final->op2 = neg1;
  final->op1 = dispatch(expr->op1);


  return (void*) final;
}

void* derivTan(Expr* expr) {
  Expr* cos = malloc(sizeof(Expr));
  cos->valType = EXPR;
  cos->operator = COS;
  cos->op1 = expr->op1;



  Expr* pow2 = malloc(sizeof(Expr));
  pow2->valType = EXPR;
  pow2->operator = POW;
  Literal* uu = malloc(sizeof(Literal));
  uu->valType = LITERAL;
  uu->type = NUMBER;
  uu->value.number = 2;
  pow2->op1 = cos;
  pow2->op2 = uu;


  Expr* final = malloc(sizeof(Expr));
  final->valType = EXPR;
  final->operator = SLASH;

  final->op1 = dispatch(expr->op1);
  final->op2 = pow2;


  return (void*) final;
}

void* derivLn(Expr* expr) {
  Expr* denominator = expr->op1;
  Expr* numerator = dispatch(expr->op1);

  Expr* final = malloc(sizeof(Expr));
  final->valType = EXPR;
  final->operator = SLASH;

  final->op1 = numerator;
  final->op2 = denominator;

  return (void*) final;
}

void* derivExp(Expr* expr) {
  Expr* final = malloc(sizeof(Expr));
  final->valType = EXPR;
  final->operator = STAR;

  final->op2 = expr;
  final->op1 = dispatch(expr->op1);

  return (void*) final;
}




void printAST(void* exprOrLiteral) {
  if(exprOrLiteral == NULL) return;
  ValType type = *((ValType*) exprOrLiteral);


  if(type == EXPR) {
    Expr* expr = (Expr*) exprOrLiteral;
    TokenType operator = *(((TokenType*) exprOrLiteral) + 1);

    printf("(");

    Token currentToken = {.type = operator, .value.number = 0};
    Token tokens[1];
    tokens[0] = currentToken;

    printTokens((TokensList) {tokens, 1});

    printAST(expr->op1);
    if(expr->op2) printf(" ");
    printAST(expr->op2);
    printf(")");
  }

  if(type == LITERAL) {
    Literal* literal = (Literal*) exprOrLiteral;
    TokenType varOrNum = *(((TokenType*) exprOrLiteral) + 1);

    Token currentToken = {.type = varOrNum, .value.number = literal->value.number};
    Token tokens[1];
    tokens[0] = currentToken;
    printTokens((TokensList) {tokens, 1});

    return;
  }
}

bool isDigit(char c) {return c >= '0' && c <= '9';}

TokensList tokenize(const char* expr) {
  static int sz = 8;
  static float scaleFactor = 1.5f;

  int len = strlen(expr);

  Token* tokens = malloc(sizeof(Token) * sz);
  size_t idx = 0;

  for(int i = 0; i < len; i++) {
    char ch = expr[i];

    switch(ch) {
        case ' ': continue;
        case '(': tokens[idx].type = LEFT_PAREN; break;
        case ')': tokens[idx].type = RIGHT_PAREN; break;
        case '+': tokens[idx].type = PLUS; break;
        case '-': tokens[idx].type = MINUS; break;
        case '*': tokens[idx].type = STAR; break;
        case '/': tokens[idx].type = SLASH; break;
        case '^': tokens[idx].type = POW; break;
        case 'x': tokens[idx].type = VAR; break;
        case 's': tokens[idx].type = SIN; i++; i++; break;
        case 'c': tokens[idx].type = COS; i++; i++; break;
        case 't': tokens[idx].type = TAN; i++; i++; break;
        case 'l': tokens[idx].type = LN;  i++;    ; break;
        case 'e': tokens[idx].type = EXP; i++; i++; break;
    }

    if(isDigit(ch)) {
      int start = i++;
      while(isDigit(expr[i])) i++;

      // check for a decimal point
      char peek = i < len ? expr[i] : 0;
      if(peek == '.') {
        i++;
        while(isDigit(expr[i])) i++;
        --i;
      } else {
        --i;
      }

      char buf[16] = {0};
      tokens[idx].type = NUMBER;
      tokens[idx].value.number = strtod(strncpy(buf, expr + start, i - start + 1), NULL);
    }

    idx++;
    if(idx >= sz) {
      sz *= scaleFactor; // Grow the tokens capacity.
      tokens = realloc(tokens, sz * sizeof(Token));
    }

  }

  return (TokensList) {tokens, idx};
}

void printTokens(TokensList tokens) {
  for(int i = 0; i < tokens.size; i++) {
    Token token = tokens.tokens[i];

    switch(token.type) {
        case PLUS: printf("+ "); break;
        case MINUS: printf("- "); break;
        case SLASH: printf("/ "); break;
        case STAR: printf("* "); break;
        case POW: printf("^ "); break;
        case LEFT_PAREN: printf("("); break;
        case RIGHT_PAREN: printf(")"); break;
        case NUMBER: printf("%f", token.value.number); break;
        case VAR: printf("x"); break;
        case SIN: printf("sin "); break;
        case COS: printf("cos "); break;
        case TAN: printf("tan "); break;
        case EXP: printf("exp "); break;
        case LN: printf("ln "); break;
    }

  }
  return;
}

void* parse(TokensList t, int* idx, int sz) {
  return expr(t, idx, sz);
}

void* expr(TokensList t, int* idx, int sz) {

  if(*idx < sz && t.tokens[*idx].type == LEFT_PAREN) {
    *idx = *idx + 1; // advance

    TokenType operator = t.tokens[*idx].type;
    *idx = *idx + 1; // advance

    Expr* newExpr = malloc(sizeof(Expr));
    newExpr->valType = EXPR;
    newExpr->operator = operator;

    newExpr->op1 = newExpr->op2 = NULL;

    switch(operator) {
        case PLUS: newExpr->interpretThyself = derivAdd; break;
        case MINUS: newExpr->interpretThyself = derivSub; break;
        case STAR: newExpr->interpretThyself = derivMult; break;
        case SLASH: newExpr->interpretThyself = derivQuot; break;
        //case POW: newExpr->interpretThyself = derivPow; break;
        case SIN: newExpr->interpretThyself = derivSin; break;
        case COS: newExpr->interpretThyself = derivCos; break;
        case TAN: newExpr->interpretThyself = derivTan; break;
        case LN: newExpr->interpretThyself = derivLn; break;
        case EXP: newExpr->interpretThyself = derivExp; break;
    }

    if(*idx < sz && t.tokens[*idx].type != RIGHT_PAREN) {
      newExpr->op1 = operand(t, idx, sz);
      ValType type = *((ValType*) newExpr->op1); // Type of value we are dealing with i.e. an expr or a literal.
      //newExpr->op1Type = type;
    }

    if(*idx < sz && t.tokens[*idx].type != RIGHT_PAREN) {
      newExpr->op2 = operand(t, idx, sz);
      ValType type = *((ValType*) newExpr->op2); // Type of value we are dealing with i.e. an expr or a literal.
      //newExpr->op2Type = type;
    }


    return (void*) newExpr;
  }

  return operand(t, idx, sz);
}

void* operand(TokensList t, int* idx, int sz) {
  if(*idx < sz && t.tokens[*idx].type == NUMBER) {
    Literal* literal = malloc(sizeof(Literal));
    literal->valType = LITERAL;
    literal->type = NUMBER;
    literal->value.number = t.tokens[*idx].value.number;
    literal->interpretThyself = (void*)derivNum;
    *idx = *idx + 1;
    return (void*) literal;
  }

  if(*idx < sz && t.tokens[*idx].type == VAR) {
    Literal* literal = malloc(sizeof(Literal));
    literal->valType = LITERAL;
    literal->type = VAR;
    literal->interpretThyself =(void*) derivVar;
    *idx = *idx + 1;
    return (void*) literal;
  }

  if(*idx < sz && t.tokens[*idx].type == LEFT_PAREN) {
    void* newExpr = expr(t, idx, sz);
    *idx = *idx + 1;
    return newExpr;
  }

  return NULL;
}


int main() {
    printf("Hello world!\n");
    return 0;
}
