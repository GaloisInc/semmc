This repository contains implementations of two different approaches to synthesis for machine code.  The first is *stratified* synthesis (based on a 2016 paper [PLDI16]_), which learns the semantics of machine code instructions from a manually-specified base set.  The second is a search-based synthesis of machine code programs from semantics using a technique called Counter-Example Guided Inductive Synthesis (CEGIS) [TR1814]_, [OOPSLA16]_.


.. [PLDI16] https://cs.stanford.edu/people/eschkufz/docs/pldi_16.pdf
.. [TR1814] http://research.cs.wisc.edu/wpis/papers/tr1814.pdf
.. [OOPSLA16] http://pages.cs.wisc.edu/~venk/papers/oopsla16b.pdf
