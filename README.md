# iohk-interview-abyaly

## Node list

The node list is located in app/Nodes.hs


## Approach

### Partial Sums

This program calculates the sum from 1 to k of (i\*m\_i). This is done by lifting each number m into a PartialSum type applying the associated monoid addition on the values in order.

The type in question:
```
data PartialSum = PartialSum { offset :: !Integer, shift :: !Pico, total :: !Pico }

fromDouble :: Double -> PartialSum
fromDouble d = let p = fromRational $ toRational d in
   PartialSum { offset = 1, shift = p, total = p }

instance Monoid PartialSum where
  mempty = PartialSum 0 0 0
  mappend a b =
    PartialSum {
      offset = offset a + offset b,
      shift = shift a + shift b,
      total = total a + (fromIntegral (offset a) * shift b) + total b
    }
```

For example, if we have [m\_1,m\_2,m\_3] = [3,4,5], then applying fromDouble to each of them yields:

- PartialSum 1 3 3
- PartialSum 1 4 4
- PartialSum 1 5 5

respectively.

Then the sum `((PartialSum 1 3 3 <> PartialSum 1 4 4) <> PartialSum 1 5 5)` evaluates to
```
PartialSum (1 + 1) (3 + 4) (3 + 1*4 + 4) <> PartialSum 1 5 5
= PartialSum 2 7 11 <> PartialSum 1 5 5
= PartialSum (2 + 1) (7 + 5) (11 + 2*5 + 5)
= PartialSum 3 12 26.
```

The value 26 is equal to 1\*3 + 2\*4 + 3\*5, as desired.

Adding a new value x onto the end of a partial sum always increases the total by (n\*x + x), where
n is the number of values that were previously included, so adding numbers from left to right
always produces a total equal to the sum from 1 to k of (i\*m\_i).

At this point one might wonder if adding in a right-associative fashion changes anything. For example:
```
(PartialSum 1 3 3 <> (PartialSum 1 4 4 <> PartialSum 1 5 5))
= PartialSum 1 3 3 <> PartialSum (1 + 1) (4 + 5) (4 + 1*5 + 5)
= PartialSum 1 3 3 <> PartialSum 2 9 14
= PartialSum (1 + 2) (3 + 9) (3 + 1*9 + 14)
= PartialSum 3 12 26.
```
In this example we see that it did not. It never does; the operation is associative. The motivated reader may write out a proof of the associativity by replacing the numbers in the above examples with symbols and observing that they resolve to the same expressions at the end. 

Alternatively, note that the provided operation emerges from a semidirect product of A and B,
```
  data A = A { offset :: Integer }
  data B = B { shift :: Pico, total :: Pico }
```
where A acts on B via `(A offset) *** (B shift total) = (B shift (offset*shift + total))`. Since a semidirect product is a group, the operation is associative.

### The purpose of assembling partial sums

The assignment requires us to decide on an order for doubles that were generated on different nodes, and then calculate the sum from 1 to k of (i\*m\_i) based on that order. 
The only constraint put on the final order is that, for each node N, of N generated m\_i before m\_j, then m\_i appears before m\_j in the final order. We are otherwise free to reorder the doubles as we see fit.

The final order I chose was the following:
  - Order the nodes alphabetically.
  - All of the doubles from the first node occur before all of the messages from the second node.
  - All of the doubles from the second node occur before all of the messages from the third node.
  - And so on.

Since the doubles generated by each node are consecutive in the final order, each node may calculate a partial sum (as above) which it shares with the cluster. Then the partial sums are combined at the end to reflect my chosen ordering.
For example, if there are two nodes, A and B, which generate the following values
```
A_1 = 1
A_2 = 2
A_2 = 1

B_1 = 1
B_2 = 1
B_3 = 1
```
then A computes `PartialSum 3 4 8` and B computes `PartialSum 3 3 6`. Combining them yields `PartialSum 6 7 23`. Since the combination operation is associative, the total of 23 corresponds to `1*A_1 + 2*A_2 + 3*A_3 + 4*B_1 + 5*B_2 + 6*B_3`.

The addition rule for the partial sum type permits node A to continue creating more doubles which will end up in the middle of the final order without disrupting the calculation.

In fact, A and B can both continue to expand their own chunks of the final sum without needing to hear from each other, as long as they learn each others' correct totals by the end.

### Communication

The choices made so far make the constraints on communication relatively lax.

We need, by the end of the program's run, for each node to learn the partial sum of each other node. We also need for each node to become confident that the other nodes know the same it does.

If the above is accomplished, then each node can combine the partial sums using addition rule and arrive at the answer.



## Thoughts

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

