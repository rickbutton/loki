(define-library (loki core primitives)
  
  (export
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   include include-ci
   _ ... syntax syntax-case

   (rename let primitive-let)

   ;; Procedures and values defined in core expander:
   
   (rename ex:identifier?               identifier?)
   (rename ex:bound-identifier=?        bound-identifier=?)
   (rename ex:free-identifier=?         free-identifier=?)
   (rename ex:generate-temporaries      generate-temporaries)
   (rename ex:datum->syntax             datum->syntax)
   (rename ex:syntax->datum             syntax->datum)
   (rename ex:syntax->source            syntax->source)
   (rename ex:source-file               source-file)
   (rename ex:source-line               source-line)
   (rename ex:source-column             source-column)
   (rename ex:syntax-violation          syntax-violation)
   (rename ex:features                  features)
   (rename ex:environment               environment)
   (rename ex:environment-bindings      environment-bindings)
   (rename ex:eval                      eval)
   (rename ex:load                      load))
  
  (import
   
   (for (only (loki core primitive-macros)
     
     begin if set! and or lambda let quote
     define define-syntax let-syntax letrec-syntax 
     include include-ci
     syntax syntax-case _ ...) run expand)
   
   ;; An extension to the import syntax, used here to make  
   ;; available variable bindings provided natively.
   ;; This will not work for macros, which have to be defined
   ;; within the context of this expander.  
   
   (primitives
   
    ;; Procedures and values defined in the core expander:
    
    ex:identifier? ex:bound-identifier=?
    ex:free-identifier=? ex:generate-temporaries ex:datum->syntax ex:syntax->datum 
    ex:syntax->source ex:source-file ex:source-line ex:source-column
    ex:syntax-violation ex:environment ex:environment-bindings ex:eval ex:load ex:features
    ))
)
