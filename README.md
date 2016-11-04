Won't work with Stack on OSX as `readline` libs get installed to a non-standard location and there's currently
no way to specify the `--configure-option` flags

Setup (OSX):
```
brew install readline
export RLPREFIX=`brew --prefix readline`
cabal install readline --extra-include-dirs=$RLPREFIX/include \
                       --extra-lib-dirs=$RLPREFIX/lib         \
                       --configure-option=--with-readline-includes=$RLPREFIX/include/readline \
                       --configure-option=--with-readline-libraries=$RLPREFIX/lib
cabal install
```

Setup (Linux)
```
your-package-manager install libreadline-dev
cabal install
```

Setup (Windows)
```
gl hf dd 
```

---

To run (anywhere)
```
cabal run server
# in another window
# -a and -n are required
cabal run client -- -a localhost -n yournick
```

---

To use (once connected)
```
Just type a message to to send it to current focused channel/user
// to escape first slash at start of regular message

/st user|#channel        -> changes current focused channel/user
/join #channel           -> joins channel
/part #channel [reason]  -> leaves #channel with optional reason
/quit [reason]           -> disconnects from server with optional reason
/nick newName            -> change your username
/say target message ...  -> sends message to target, and changes target to current focus for subsequent messages
/msg   ""     ""         -> ditto
/me third person action  -> broadcasts a message in third person form (e.g. * yrname msg instead of <yrname> msg)
/act ""                  -> ditto
```
