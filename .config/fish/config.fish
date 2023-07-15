# -----------------------
#     Startup
# -----------------------

if status is-interactive
    # prompt
    if type -q starship
        starship init fish | source
    end
    # theme
    fish_config theme choose 'default' 2>/dev/null
    # show a random litte pokemon
    if type -q krabby
        set -l pokemon (mktemp /tmp/pokemon.XXXXXX)
        while krabby random --no-title >$pokemon; and test (count <$pokemon) -gt 14; end; cat $pokemon; rm $pokemon
    end
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

set -g fish_greeting      # disable fish greeting