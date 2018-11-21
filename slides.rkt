#lang at-exp slideshow

(define (pretty-table columns elements)
  (table
    (length columns)
    (append (map t columns)
            (map t elements))
    lc-superimpose
    cc-superimpose
    30
    10))

(slide #:title "Why JSON when you can DSL?"
  @t{Creating file formats & languages that fit your needs}
  @t{Jérôme Martin})

(slide #:title "Horror stories"
  'alts
  (list
    (list
      @t{Lua files used as data}
      @para{It started as arrays only but soon became a monster}
      'next
      @item{It was doing shady stuff when executed (think: checking conditions from global variables)}
      @item{It was difficult to parse}
      @item{We had to write a whole custom Lua parser that would detect some patterns and apply regexps})

    (list
      @t{Grunt files}
      @para{Same story: it started as a good idea but usually become a humongous blob of custom code}
      'next
      @item{Start with a base of packaging code (copy/paste and replace stuff around)}
      @item{Add a layer for i18n}
      @item{Add a layer for calling linters}
      @item{Add a layer for handling dev/prod envs}
      @item{Add a layer of sugar}
      @item{Bon appetit!})

    (list
      @t{Javascript inside CouchDB}
      'next
      @item{No comments here (sorry couchdb users!)}
      @item{Javascript in <not a browser> is a bad idea anyways (Gnome 3 anyone?)})))

(slide #:title "Here comes the DSL!"
  'alts
  (list
    (list
      @t{Domain Specific Language}
      @para{A data structure is just a stupid programming language.
            -- Bill Gosper}
      'next
      @t{Different types of DSLs:}
      'next
      @item{Declarative (HTML, Glade, WPF)}
      'next
      @item{Query-style (SQL, GraphQL, XPath, Regexp)}
      'next
      @item{Libraries (SDL, Qt, Box2D)}
      'next
      @item{Complex tasks (Makefile, Verilog, GameMaker Language)})

    (list
      (pretty-table
        '("Languages"       "Domain")
        '("HTML, CSS"       "Web pages"
          "Glade, WPF, Qt"  "Application Interface"
          "SQL, GraphQL"    "Database fetching"
          "Regexp"          "Text filtering"
          "SDL, OpenGL"     "Pixel rendering"
          "Box2D, ODE"      "Physics simulation"
          "Makefile"        "File cache invalidation"
          "Verilog"         "Hardware design and simulation"
          "GML"             "Gameplay & actor behaviour")))))

(slide #:title "Why would I make my own?"
  @t{Pros:}
  @item{Built inside a business context}
  @item{Fit your instant needs without having to learn how to use it}
  @item{Forces you to describe your business in simple terms}
  @item{Extendable at will}
  @t{Cons:}
  @item{Usually not documented}
  @item{No possible former experience from newcomers}
  @item{Can become useless if the project it served is canceled}
  @item{Difficulty to work in teams if designed by a few})

(slide #:title "Main argument against DSLs:"
  @para{But newcomers will have no previous expertise of the language!}
  @t{Answer:}
  @para{Language knowledge has always been irrelevant to the productivity of developers in a new team.}
  @para{Domain knowledge is relevant.}
  @t{Example:}
  @para{A new frontend developer starts working in the bank industry.
        His javascript knowledge means nothing compared to his ability to understand banking.
        Therefore, a language that explains banking with precision is more valuable for him
        and will be easier to learn than discovering a big ball of javascript that somehow "does" banking.})

(slide #:title "When to go for a DSL?"
  @t{If you...}
  @item{work in a specific domain}
  @item{need to describe data, processes or systems}
  @item{are sure that the problem you are trying to solve is gonna stick around for a long time}
  @t{then make a DSL!})

(slide #:title "Your DSL is already there!")
#|
How to design a DSL?

    Just use it first. But it doesn't exist! Just write down how you would use it if it existed. Just explain your need.

    Be confortable with a way of writing it (its syntax). Some suggestions, from easy to hard:
        s-expressions
        one line per instruction
        C-like
        plain English

    Try to find other usecases and see how they fit in what you already wrote. Rework some old parts.

    Notice we didn't write any parsing code yet. Show your files to future users and get their feedbacks. Try to see if they can understand your intention just from reading your code.

    The idea seems good now, let's write the parser!
        If you chose s-exprs, your parser is done.
        If you chose one-line per instructions, your parser is easy to write in about 5 lines of Racket.
        If you chose a more programming language style, use brag.
        If you chose plain English.. Come on, you should be at the AI workshop by now.

    The elements of a DSL: Reader > Parser > Expander
        Reader: cut a text stream in parts
        Parser: give meaning to those parts
        Expander: generate actual code

|#

(slide #:title "Racket: the DSL factory")

(slide #:title "Join us and try making your own!")
