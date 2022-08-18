#!/bin/bash
set -e
trap '[!] Error ${?} at line ${LINENO}' ERR
echo "[*] Preparing the system using BIOS..."

# check root privileges
SUDO=''
if test "${EUID}" -ne 0; then
SUDO='sudo'
fi

# get the disk name
disk=$(lsblk | grep disk | sort -grk 4 | head -1 | cut -f1 -d' ')

# partition the disk using UEFI
echo "[*] Partitioning /dev/${disk}..."
commands=( "mklabel msdos" "mkpart primary linux-swap 512MiB 8GiB" "mkpart primary 8GiB 100%" )
for command in "${commands[@]}"; do
    yes | "${SUDO}" parted "/dev/${disk}" -- "${command}" &>/dev/null
done

# format the partitions
partitions=$(lsblk | grep part | grep sda | cut -d' ' -f1 | sed 's/^.\{2\}//g')
echo "[*] Formatting partitions..."
while IFS= read -r partition; do
    case "${partition}" in
        *1) "${SUDO}" mkswap -L SWAP "/dev/${partition}" &>/dev/null ;;
        *2) "${SUDO}" mkfs.ext4 -L SYSTEM "/dev/${partition}" &>/dev/null ;;
    esac
done < <(printf '%s\n' "${partitions}")
sleep 1

# enable swap
echo '[*] Enabling SWAP...'
"${SUDO}" swapon /dev/disk/by-label/SWAP

# mount the partitions
echo "[*] Mounting SYSTEM and BOOT devices..."
"${SUDO}" mount /dev/disk/by-label/SYSTEM /mnt/

# download the flake file
echo "[*] Cloning the NixOS configuration files..."
"${SUDO}" mkdir -p /mnt/etc
"${SUDO}" git clone https://github.com/cosasdepuma/dotfiles /mnt/etc/nixos &>/dev/null

echo "[+] System prepared!"