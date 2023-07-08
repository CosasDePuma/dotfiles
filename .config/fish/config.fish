# -----------------------
#     Startup
# -----------------------

if status is-interactive
    # prompt
    starship init fish | source
    # show a random litte pokemon
    set -l pokemon (mktemp /tmp/pokemon.XXXXXX)
    while krabby random --no-title >$pokemon; and test (count <$pokemon) -gt 14; end; cat $pokemon; rm $pokemon
end

# -----------------------
#     Environment
# -----------------------

switch "$TERM_EMULATOR"
    case '*kitty*'
        export TERM='xterm-kitty'
    case '*'
        export TERM='xterm-256color'
end

# -----------------------
#     Configuration
# -----------------------

set -U fish_greeting      # disable fish greeting