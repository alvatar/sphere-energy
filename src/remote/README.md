# Files in this directory

* debug-server.scm: Scheme code that runs a server that will spawn a REPL once the debuggee sends a signal
* debuggee.scm: Scheme code able to signal the server to spawn a REPL
* pump.scm: Scheme code needed by the spawned REPLs
* rdi.scm: common client/server Scheme code
* sense: command-line utility to create the server
* sense-emacs: command-line utility to create the server within Emacs (called from sense-emacs.el)
* sense-emacs.el: Emacs module for creating a Sense server and REPLs within the Emacs environment
* sense-pump: command-line utility to spawn a REPL (used by the server)
