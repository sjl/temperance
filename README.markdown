    ,--,--'
    `- | ,-. ,-,-. ,-. ,-. ,-. ,-. ,-. ,-. ,-.
     , | |-' | | | | | |-' |   ,-| | | |   |-'
     `-' `-' ' ' ' |-' `-' '   `-^ ' ' `-' `-'
                   |
                   '

Temperance is a logic programming library for Common Lisp.

**Temperance is still in development, don't actually try to use this for
anything important yet.**

![I don't know what that means](https://i.imgur.com/EWPGAHa.gif)

Temperance is an implementation of the [Warren Abstract Machine][wam] in Common
Lisp, designed to let you write Lispy Prolog.  Its main goals are:

* Be fast.
* Implement a decent subset of vanilla Prolog.
* Execute quickly.
* Interop back and forth with Common Lisp.
* Don't be slow.

Temperance was made with [General Game Playing][ggp] in mind (hence the focus on
performance), but should be useful for anything you might normally use Prolog
for.

[wam]: https://en.wikipedia.org/wiki/Warren_Abstract_Machine
[ggp]: https://en.wikipedia.org/wiki/General_game_playing

* **License:** MIT
* **Documentation:** <https://docs.stevelosh.com/temperance/>
* **Mercurial:** <https://hg.sr.ht/~sjl/temperance/>
* **Git:** <https://github.com/sjl/temperance/>
