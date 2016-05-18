# I'm a lazy developer, so I try to automate everything.
# I can't be bothered to manually check my work as I go
#
# My dev cycle
#
# 1. write some unit tests
# 2. implement the function
# 3. see if the tests pass
# 4. loop
#
# I find this is nicer than typing the same commands into the REPL repeatedly.
#
# This often involves a lot of switching to another terminal to run tests.
# I'm so lazy that this small amount of input gets annoying, so where possible
# I get the computer to do it for me.
#
# Most languages will a good testing culture will have a tool that will
# automatically run your tests for you each time you save a source file in your
# project.
#
# Javascript has Karma, Grunt, and Gulp.
# Haskell has Stack
# Ruby has Guard.
# Erlang has the rebar3 autotest plugin.
#
# Back at the start of 2015 Elixir had...? Nothing.
# I'll have to write one.
#
# The first version was pretty horrible, but it worked, and I've been using it
# almost every day since.
# I'm going to walk you through roughly how it works today.
# Hopefully you'll have some thoughts as to how we can make it better.
