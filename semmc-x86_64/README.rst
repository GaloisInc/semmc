This package currently contains a demonstration of using the remote runner to run test vectors on remote hosts.

It supports the integer registers for x86_64.  The defined machine state (see SemMC.X86) has space for the floating point and vector registers, but the remote runner executable doesn't currently populate them.

To run this binary, build the remote runner and put it in the ``PATH`` on the remote machine.  Invoke the test binary (``semmc-x86-test``) with the remote hostname as its argument.  The test binary currently sends two machine states over the network and prints the results when they come back.
