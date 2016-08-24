Temperance GDL Reasoning Benchmark
==================================

An thin wrapper to plug Temperance into [Stephan and Yngvi's GDL reasoner
benchmarking library thing][paper].

[paper]: http://cgi.cse.unsw.edu.au/~mit/GGP/GIGA-13-Proceedings.pdf#page=55

Setup
-----

Install [Roswell][].

Pick a Lisp implementation and install it with Roswell.  Temperance is primarily
focused on running fast with [SBCL][], but should run on at least CCL and ECL
too.

    ros install sbcl

Clone down the dependencies into Roswell's `local-projects` directory.  You can
clone them elsewhere and symlink them if you prefer:

    cd ~/.roswell/local-projects/

    # Mercurial
    hg clone http://bitbucket.org/sjl/temperance
    hg clone http://bitbucket.org/sjl/cl-losh

    # Git
    git clone http://github.com/sjl/temperance
    git clone http://github.com/sjl/cl-losh

Build the benchmarking binary with your chosen Lisp:

    cd ~/.roswell/local-projects/temperance/contrib/gdl-benchmark/

    ros use sbcl
    ros build run-temperance.ros

Run the binary just like you would any of the others in the suite:

    ~/.roswell/local-projects/temperance/contrib/gdl-benchmark/run-temperance 'dfs mc' 10 .../foo.gdl .../foo.trace

[Roswell]: https://github.com/roswell/roswell
[SBCL]: http://www.sbcl.org/

Performance
-----------

The benchmark script ensures that Temperance will be compiled with sane
optimization settings: `(debug 1) (safety 1) (speed 3)`.

If you want to throw caution to the wind and see how fast it can get, you can
set the `PLEASE_SEGFAULT` environment variable to `YES` **when building**:

    cd ~/.roswell/local-projects/temperance/contrib/gdl-benchmark/

    ros use sbcl
    PLEASE_SEGFAULT=YES ros build run-temperance.ros

This must be done when *building*.  The variable has no effect when running the
binary.

In practice this results in a speed increase of around 20%.
