# Intel 8080 Emulator

A Clojure library that can be used to emulate the behavior of an Intel 8080 microprocessor.
Pass in the memory and IO ports and let it run!

WARNING!! This is work in progress! WARNING!!


## Origins

I have been toying with Clojure and have built several small programs with it. 
As much fun as it was, it did not help me in better understanding the power of functional languages.
So I decided to build something more substantial and thought that a microprocessor emulator would be a good project.
There should be enough repetition in it that would require some structure and I am curious about the challenge
of using stateless structures to represent what is essentially a mutable state machine.

As for the choice of processor to emulate, I wanted something small enough to be implemented in a reasonable amount of time.
I built Intel8080-based systems when I was back in college, so it was a natural candidate.
And I have to add that re-reading the microprocessor manual after such a long time was a blast!






## License

Copyright © 2023 santoro63@yahoo.com

This program and the accompanying materials are made available under the
terms of the GNU Lesser General Public License v3, which is available at
https://www.gnu.org/licenses/lgpl-3.0.html
