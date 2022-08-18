# .---------------.
# |  GENERATIONS  |
# '---------------'

# 🔀 switch to the latests configuration
.PHONY: switch
switch: prebuild
	nixos-rebuild switch --flake .#$(shell hostname)

# 🌔 switch to the latests configuration (forcing boot changes)
.PHONY: boot
boot: prebuild
	nixos-rebuild boot --flake .#$(shell hostname)

# 🔨 prerequisites to rebuild the system
.PHONY: rebuild
prebuild: .git flake.nix flake.lock
	nix flake update
	git add .
	git diff-index --quiet HEAD || git commit -m "Switching generation"

# ⏮️ switch to an old generation
.PHONY: rollback
rollback:
ifeq ($(version),)
	nix-env --rollback
else
	nix-env --switch-generation $(version)
endif

# 📜 list all the generations
.PHONY: list
list:
	nix-env --list-generations


# .---------------.
# |    UPDATES    |
# '---------------'

# ⤵️ fetch the latests configuration
.PHONY: fetch
fetch: .git
	git fetch
	git reset --hard origin/main

# 🚀 update the system
.PHONY: update
update: ~/.nix-channels
	nix-channel --update


# .---------------.
# |    GARBAGE    |
# '---------------'

# 👤 remove all the user programs
.PHONY: uninstall
uninstall:
	nix-env -e "*"

# 🔗 remove all broken symlinks in gcroots
.PHONY: broken
broken:
	find /nix/var/nix/gcroots/ -xtype l -delete
	/run/current-system/bin/switch-to-configuration boot

# 🚮 remove all the garbage files
.PHONY: clean
clean:
	nix-collect-garbage -d

# 🔥 remove all
.PHONY: prune purge
purge: prune
prune: | uninstall broken clean

# .---------------.
# |     FILES     |
# '---------------'

flake.lock: flake.nix
	nix flake lock
