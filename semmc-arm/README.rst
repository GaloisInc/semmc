This package is used for statically defining and/or dynamically
learning the semantics of ARM processor instructions, both ARM/AArch32
(a.k.a. A32) and Thumb (a.k.a. T32, which largely consists of
instructions using a 16-bit Thumb encoding, but which execute in a
32-bit memory space).

The semantics are defined in terms of effects on Memory and local
processor registers; any unusual effects on processor state
(e.g. wait-for-event, low-power, syscall) are deferred to higher-level
handling.

To generate static semantics definitions, create the static definition
and then run the ``semmc-arm-genbase`` executable (see the
`BaseSemantics README <src/SemMC/Architecture/ARM/BaseSemantics/README.org>` file.

To dynamically learn semantics:

  #. Build the remote runner
  #. Put the remote runner it in the ``PATH`` on the remote machine
  #. Invoke the test binary (``semmc-arm-test``) with the remote hostname as its argument.

.. warning:: The ``semmc-arm-test`` binary is not currently fully implemented.

The test binary currently sends two machine states over the network
and prints the results when they come back.
