#lang slideshow
(require slideshow/text)
(require unstable/gui/slideshow)
(require (except-in plot/pict shade))

(current-main-font "Roboto Sans")

(define (graphviz . str)
  (let* ((f (make-temporary-file "rkttmp~a.png"))
         (cmd (~a "printf \"" (string-replace (apply string-append str) "\""
                                              "\\\"")  "\" | dot -Tpng > " f)))
    (display cmd)
    (newline)
    (system cmd)
    (bitmap (~a f))))

(slide
  (bold (t "How to build a programming language (WIPS)")))

(slide
  (t "What is a programming language?")
  'next
  (item "Formal language")
  'next
  (item "Programmer to machine")
  'next
  (item "Programmer to programmer"))

(slide
  (t "Why should you care?")
  'next
  (item "Reason about errors")
  'next
  (item "Understand performance")
  'next
  (item "Build your own!"))

(slide
  (bitmap "progcat.jpg"))

(slide
  (t "Types in languages")
  (t "Dynamic vs static")
  (t "Strong vs weak"))

(slide
  (t "Types in languages")
  (parameterize ((plot-width    800)
                 (plot-height   600)
                 (plot-font-size 30)
                 (plot-font-face "Liberation Sans")
                 (plot-x-label  "Weak ↔ Strong")
                 (plot-y-label  "Dynamic ↔ Static"))
    (define xs (build-list 20 (λ _ (random))))
    (define ys (build-list 20 (λ _ (random))))
    (plot '()
          #:x-min -1.1
          #:x-max 1.3
          #:y-min -1.1
          #:y-max 1.1)))

(slide
  (t "Types in languages")
  (parameterize ((plot-width    800)
                 (plot-height   600)
                 (plot-font-size 30)
                 (plot-font-face "Liberation Sans")
                 (plot-x-label  "Weak ↔ Strong")
                 (plot-y-label  "Dynamic ↔ Static"))
    (define xs (build-list 20 (λ _ (random))))
    (define ys (build-list 20 (λ _ (random))))
    (plot (list
            (point-label (vector -1 0.5) "C")
            (point-label (vector -0.5 0.7) "C++")
            (point-label (vector 0.7 -1) "Python")
            (point-label (vector -0.8 -1) "JavaScript")
            (point-label (vector 1 1) "SML"))
          #:x-min -1.1
          #:x-max 1.3
          #:y-min -1.1
          #:y-max 1.1)))

(revealing-slide
  (two-columns
    (vc-append 20
      (bt "Interpreter")
      (reveal 1 (item "Dynamically executes code"))
      (reveal 2 (item "Slow"))
      (reveal 3 (item "More debug/runtime info")))
    (vc-append 20
      (bt "Compiler")
      (reveal 1 (item "Generates executable"))
      (reveal 2 (item "Fast"))
      (reveal 3 (item "Less debug/runtime info (unless explicitely added)")))))

(slide
  (bt "Parsing"))

(slide
  (para "The process of semantically converting syntax to a structure using a formal grammar"))

(slide
  (item (tt "lex") "+" (tt "yacc"))
  (item "Parser combinator"))

(slide
  (para #:align 'center (tt "lex") (t "+") (tt "yacc"))
  (item "Standard for modern implementations")
  (item "Old approach"))

(slide
  (tt "lex")
  (item "Tokenizes")
  (item "Tags"))

(slide
  (vl-append
    (tt "int main(int argv, char **argv) {")
    (tt "  int x = rand_number();")
    (tt "  printf(\"%d\", x);")
    (tt "  return 0;")
    (tt "}"))
  (para "→ ..., id{'int'}, id{'x'}, equal, id{'rand_number'}, lparen, rparen, semi, ..."))

(slide
  (t "lexing is a minimal preprocessing pass"))

(slide
  (para "Parsing (with" (tt "yacc") ")"))

(slide
  (para (tt "yacc") "converts stream of tokens to" (italic (t "parse tree"))))

(slide
  (para "for instance, the stream before would become"))

(slide
  (graphviz "digraph G {
            size = \"8,8\";
            ordering=out;
            node [shape = box];
            a [label=\"VARIABLE DECLARATION (int)\"];
            b [label=\"NAME: x\"];
            c [label=\"FCALL: rand_number\"];
            d [label=\"ARGS: []\"];
            a -> b;
            a -> c -> d;
            }"))

(slide
  (t "Parser combinator"))

(slide
  (para "Functional solution to the problem")
  (para "Uses " (italic "higher order functions") " to compose parser functions together"))

(slide
  (tt "parse(char, \"woot\") => 'w'")
  (tt "parse(num, \"13\") => 3")
  'next
  (tt "parse(num, \"31\") => error ..."))

(slide
  (para (tt "parse(many(char), \"hax0r\") => \"hax\""))
  'next
  (para
    (tt "parse(many(or(char, num)), \"h0w t0 b a l33t hax0r?\")")
    (tt " => \"h0w\"")))

(slide
  (bitmap "runaway.jpg"))

(slide
  (tt "Let's build an interpreter!"))

(slide
  (para "An interpreter walks the tree and executes the semantic meaning dynamically"))

(slide
  (tt "the wizard book")
  (bitmap "sicp.jpg"))

(slide
  (bitmap "eval-apply.gif"))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  if is_literal(expr):")
    (tt "    return expr")
    (tt "  ...")
    (blank-line)
    (tt "eval(4, [{}]) => 4")
    (tt "eval(\"it's lit\", [{}]) => \"it's lit\"")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_assignment(expr):")
    (tt "    v = eval(expr.rhs, environment)")
    (tt "    environment[0][expr.lhs] = v")
    (tt "    return v")
    (tt "  ...")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_variable(expr):")
    (tt "    for frame in environment:")
    (tt "      if expr in frame:")
    (tt "        return frame[expr]")
    (tt "    raise NameError(\"unbound value\")")
    (tt "  ...")
    (blank-line)
    (tt "eval(a, [{'a': 3}]) => 3")
    (tt "eval([Assign(b, 2), b], [{}]) => 2")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_variable(expr):")
    (tt "    for frame in environment:")
    (tt "      if expr in frame:")
    (tt "        return frame[expr]")
    (tt "    raise NameError(\"unbound value\")")
    (tt "  ...")
    (blank-line)
    (tt "eval(a, [{'a': 3}]) => 3")
    (tt "eval([Assign(b, 2), b], [{}]) => 2")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_lambda(expr):")
    (tt "    return Closure(expr.args, expr.body, env)")
    (tt "  ...")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_func_call(expr):")
    (tt "    return apply(expr.function, expr.args)")))

(slide
  (vl-append
    (tt "def apply(function, args):")
    (tt "  if is_func_call(expr):")
    (tt "    new_env = [dict(zip(function.argnames, args))]")
    (tt "              + f.env")
    (tt "    last = None")
    (tt "    for expr in function.body:")
    (tt "      last = eval(expr, new_env)")
    (tt "    return last")
    (blank-line)
    (tt "# equal to the python code `(lambda x: x)(3)`")
    (tt "eval(Call(Lambda([Id('x')], Id('x')), 3), [{}])")
    (tt " => 3")))

(slide
  (vl-append
    (tt "def apply(function, args):")
    (tt "  ...")
    (tt "    new_env = [dict(zip(function.argnames, args))] + f.env")
    (tt "    last = None")
    (tt "    for expr in function.body:")
    (tt "      last = eval(expr, new_env)")
    (tt "    return last")))
