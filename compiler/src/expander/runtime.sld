(define-library (expander runtime)
(import (scheme r5rs))
(import (expander compat))
(include "runtime.scm")
(export ex:load-hook-set!
        ex:unspecified
        ex:make-library
        ex:library-name
        ex:library-envs
        ex:library-exports
        ex:library-imports
        ex:library-builds
        ex:library-visiter
        ex:library-invoker
        ex:library-build
        ex:library-visited?
        ex:library-invoked?
        ex:library-visited?
        ex:library-visited?-set!
        ex:library-invoked?-set!
        ex:imported
        ex:import-libraries-for
        ex:import-libraries-for-run
        ex:register-library!
        ex:lookup-library))
