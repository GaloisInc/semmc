remote-runner: remote-runner.c
	clang -DLOGGING -Wextra -Wall -rdynamic -g remote-runner.c -o remote-runner

# The 32 bit big endian PPC build
#
# We build this as a static binary because our PPC box doesn't have a 32 bit
# userland at all, but the kernel supports 32 bit binaries.
remote-runner.ppc32: remote-runner.c
	powerpc-linux-gnu-gcc -static $< -o $@

# A 32 bit ARM build suitable for ARMv7 or AArch32
remote-runner.arm: remote-runner.c
	arm-linux-gnueabi-gcc -static $< -o $@
