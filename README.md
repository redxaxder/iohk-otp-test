# iohk-interview-abyaly

The node list is located in app/Nodes.hs

The the current state of this program can only be considered a partial solution at best.

I see these remaining problems:
  - not resilient against restarts
  - the follower nodes are still using their own clocks, instead of only acting based on a centralized clock
  - agreement over commit index is vulnerable to a race condition
  - not resilient against leader being partitioned off
  - leader memory use is not bounded, since it potentially maintains the entire log
  - in a rush, automated testing was skipped. a lot of the code is in a style that is vulnerable to typo 
    induced errors, so this is a bigger problem than usual.

I think distributed consensus is pretty difficult.  My original hope was to implement Raft (or an alternative) 
or find a library to use.  I did find a Zookeper library, but I figured a binary dependency would be foul play 
here.  I learned a bit about Raft, but ultimately concluded that I can't understand and implement it within 
the given constraints.  So this program is what we're left with.

On a more positive note, I think this may be on the path to a more flexible and complete solution.
Here are some features of this approach that I think would not be lost along the way:
  - The pace of generation of random numers is detached from the rate at which the shared log is populated,
    allowing for bigger scores.
  - The distributed consensus portion is agnostic over the choice of state machine.
  - The structure of the messages is highly redundant -- if almost all of the messages from a particular 
    follower are lost this will not necessarily reduce the score.

