(require 'term+autoloads)
(eval-after-load 'term+
  '(progn
     (require 'xterm-256color)
     (require 'term+key-intercept)
     (require 'term+mode)
     (eval-after-load 'evil '(require 'term+evil))
     (require 'term+anything-shell-history)))
