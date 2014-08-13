An implementation of Montague's PTQ in Haskell
==============================================

[![Build Status](https://secure.travis-ci.org/msakai/ptq.png?branch=master)](http://travis-ci.org/msakai/ptq)

Build and Install
-----------------

You'll need GHC 6.7 or later.

    % cabal configure
    % cabal build
    % cabal install

Usage of interactive shell ptq
------------------------------

    % ptq
    PTQ> John seeks a unicorn.
    Parsed:
      F4 john (F5 seek (F2 a unicorn))
    
    Translation:
      (\x0. x0 {john}) (Int (seek (Int ((\x0. \x1. exists x2. x0 {x2} && x1 {x2}) (Int unicorn)))))
    
    Translation (simplified):
      seek (Int (\x0. exists x1. unicorn x1 && x0 {x1})) john
    
    ------------------------------------------------------
    
    Parsed:
      F10 0 (F2 a unicorn) (F4 john (F5 seek (He 0)))
    
    Translation:
      (\x0. \x1. exists x2. x0 {x2} && x1 {x2}) (Int unicorn) (Int (\x0. (\x1. x1 {john}) (Int (seek (Int (\x1. x1 {x0}))))))
    
    Translation (simplified):
      exists x0. unicorn x0 && seek (Int (\x1. x1 {x0})) john
    PTQ> quit
    %

CGI interface
-------------

By locating ptq.cgi, cgi/index.html and cgi/main.html to the place
where CGI is executable, you can try it on the web.
Demo site runs at <http://www.tom.sfc.keio.ac.jp/~sakai/hsPTQ/>.
