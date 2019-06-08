---
title: Haskell notes
---

A place for all the things I think are cool in Haskell.

## Effects:

### Libraries:
- **[Polysemy](https://hackage.haskell.org/package/polysemy)**:
      The new hotness in free monads, aiming to bring the performance of mtl with the ease of use of free monad librariesw like freer-simple.

- **[Discrimination](http://hackage.haskell.org/package/discrimination)**:
    Linear time grouping and sorting. *Linear time sorting*. **Linear time sorting**. Using the observation that algebraic data types can be treated as the radix for a radix sort. Can be used for grouping data by key (`groupWith :: Grouping b => (a -> b) -> [a] -> [[a]]`), which will produce the lists productively (they will lazilly produce values as they are encountered in the input list, so this enables parallel processing).
  
    It took me a loing time to unpack what the definition of `Group` actually means:

  ```haskell
  newtype Group a = Group
    { getGroup 
        :: forall m b. PrimMonad m  -- IO or ST
        => (b -> m (b -> m ()))     -- Run once for each `a`, used to set 
                                    -- up whatever state is needed to
                                    -- process new `b`'s, where you get to 
                                    -- decide what `b` is. It's important 
                                    -- to make sure that the `b` you're 
                                    -- passed is also stored in whatever 
                                    -- state you need - I forgot that in my 
                                    -- initial concurrent processing codce 
                                    -- below.

        -> m (a -> b -> m ())       -- Returns an action which can be 
                                    -- applied to an a and a b which will
                                    -- select the appropriate action which
                                    -- was returned from the previous
                                    -- argument.
    } deriving Typeable
  ```

  in the implementation of `group`, `b` here is `[a]`<sup>1</sup>.

  <sup>1</sup><sub>It's actually using some crazy Ed magic to make the productive lazy values. Here be dragons with confusing types.</sub>

  Some code I wrote for using this to push data to concurrent consumers, which are spawned for each new group:

  ```haskell
  -- | Given an action which can process b;s from a stream, and a list of a's and b's, fork one processor per 'a'. All processors are sent 'Nothing' to signal there is not further data to process.
  distribute :: forall a b. Grouping a => (Chan (Maybe b) -> IO ()) -> [(a,b)] -> IO ()
  distribute fork xs = do
    -- Create somewhere to store all the chans and async to gether
    -- resuls once all elements in the list are processed.
    chans <- newMutVar ([]::[(Async (), Chan (Maybe b))]) 
    k <- getGroup (grouping @a) $ \b -> do -- Function run once for each new 'a'
      -- get the current list of Chans to add the new one to.
      allChans <- readMutVar chans 
      -- Create a new chan for the 'b''s tagged with this new 'a'
      chan <- newChan 
      -- Fork off the processor with this chan
      asyn <- async (fork chan) 
      -- Make sure we send it the first 'b' we encountered.
      writeChan chan (Just b) 
      -- Add the new chan and async handle to the list.
      writeMutVar chans $ (asyn,chan) : allChans
      -- Return a function which will add new b's for this 'a' to the 
      -- correct chan. This is the `(b -> m ()) in the `Group` definition.
      return $ \b' -> writeChan chan (Just b')
    -- k is the (a -> b -> m ()) function in the 'Group' definition.
    -- Now the input data is actually processed.
    mapM_ (uncurry k) xs 
    allChans <- readMutVar chans
    -- Tell all processors we're done.
    asyncs <- mapM (\(async,chan) -> writeChan chan Nothing $> async) allChans
    -- Wait for them all to terminate.
    mapM_ wait asyncs
  ```

  An example of using this to discriminate between 11 groups - I've added 
  ```haskell
  *Main MVar> v <- newMVar () -- ghetto locking on stdout to avoid interleaving output
  *Main MVar> let prnt xs chn = do readChan chn >>= \case Nothing -> withMVar v $ \() -> print (reverse xs); Just x ->  prnt (x:xs) chn
  *Main MVar> distribute (prnt []) $ map (\n -> let nMod11 = n `mod` 11 in (nMod11,(nMod11, n))) [1..100::Int]
  [(4,4),(4,15),(4,26),(4,37),(4,48),(4,59),(4,70),(4,81),(4,92)]
  [(5,5),(5,16),(5,27),(5,38),(5,49),(5,60),(5,71),(5,82),(5,93)]
  [(6,6),(6,17),(6,28),(6,39),(6,50),(6,61),(6,72),(6,83),(6,94)]
  [(7,7),(7,18),(7,29),(7,40),(7,51),(7,62),(7,73),(7,84),(7,95)]
  [(8,8),(8,19),(8,30),(8,41),(8,52),(8,63),(8,74),(8,85),(8,96)]
  [(9,9),(9,20),(9,31),(9,42),(9,53),(9,64),(9,75),(9,86),(9,97)]
  [(10,10),(10,21),(10,32),(10,43),(10,54),(10,65),(10,76),(10,87),(10,98)]
  [(0,11),(0,22),(0,33),(0,44),(0,55),(0,66),(0,77),(0,88),(0,99)]
  [(1,1),(1,12),(1,23),(1,34),(1,45),(1,56),(1,67),(1,78),(1,89),(1,100)]
  [(2,2),(2,13),(2,24),(2,35),(2,46),(2,57),(2,68),(2,79),(2,90)]
  [(3,3),(3,14),(3,25),(3,36),(3,47),(3,58),(3,69),(3,80),(3,91)]
  ```
  We can discriminate on constructors too!
  ```haskell
  Main MVar> distribute (prnt []) $ map (\n ->let key = if even n then Left (n`mod`4) else Right (n`mod`7) in (key,(key, n))) [1..100::Int]
  [(Right 1,1),(Right 1,15),(Right 1,29),(Right 1,43),(Right 1,57),(Right 1,71),(Right 1,85),(Right 1,99)]
  [(Left 0,4),(Left 0,8),(Left 0,12),(Left 0,16),(Left 0,20),(Left 0,24),(Left 0,28),(Left 0,32),(Left 0,36),(Left 0,40),(Left 0,44),(Left 0,48),(Left 0,52),(Left 0,56),(Left 0,60),(Left 0,64),(Left 0,68),(Left 0,72),(Left 0,76),(Left 0,80),(Left 0,84),(Left 0,88),(Left 0,92),(Left 0,96),(Left 0,100)]
  [(Right 3,3),(Right 3,17),(Right 3,31),(Right 3,45),(Right 3,59),(Right 3,73),(Right 3,87)]
  [(Left 2,2),(Left 2,6),(Left 2,10),(Left 2,14),(Left 2,18),(Left 2,22),(Left 2,26),(Left 2,30),(Left 2,34),(Left 2,38),(Left 2,42),(Left 2,46),(Left 2,50),(Left 2,54),(Left 2,58),(Left 2,62),(Left 2,66),(Left 2,70),(Left 2,74),(Left 2,78),(Left 2,82),(Left 2,86),(Left 2,90),(Left 2,94),(Left 2,98)]
  [(Right 5,5),(Right 5,19),(Right 5,33),(Right 5,47),(Right 5,61),(Right 5,75),(Right 5,89)]
  [(Right 0,7),(Right 0,21),(Right 0,35),(Right 0,49),(Right 0,63),(Right 0,77),(Right 0,91)]
  [(Right 2,9),(Right 2,23),(Right 2,37),(Right 2,51),(Right 2,65),(Right 2,79),(Right 2,93)]
  [(Right 4,11),(Right 4,25),(Right 4,39),(Right 4,53),(Right 4,67),(Right 4,81),(Right 4,95)]
  [(Right 6,13),(Right 6,27),(Right 6,41),(Right 6,55),(Right 6,69),(Right 6,83),(Right 6,97)]
  ```

  I've had some discussions with Ed about how to best implement `Grouping Text`, so far the best I've come up with is a comb ination of the `hashing :: Hashable a => Group a`  and using the `Grouping a => Grouping [a]` instance :

  ```haskell
  -- Some alternatives
  grouping :: Group Text
  -- Not good is there ar elong common prefixes
  grouping = hashing <> contraMap T.unpack grouping
  -- Might help with the above, but is weird
  grouping = contraMap (T.takeEnd 64{-arbitrary-}) hashing
            <> contraMap T.unpack grouping -- fall back to [Char]
  ```
  Ed informs me there's a proper way to do this

  ```
  -- #haskell - 2019-06-07
  ...
  18:35         Axman6: I also thought of doing something like contraMap (T.take 64) hashing <> contramap T.unpack grouping as a middle ground 
  18:35        edwardk: because give two novels you'd expect only log novel count characters worth of distinct common prefix 
  18:36        edwardk: that starts to sound like a sensible next step 
  18:36         Axman6: though that still degrades in the long common prefix case 
  18:36        edwardk: this would get a lot better in general if we did the burst-sort improvements on discrimination 
  18:37         Axman6: contraMap (T.takeEnd 64) hashing <> contramap T.unpack grouping as a middle ground -- >_> 
  18:37         Axman6: uh, s/as a middle ground// 
  18:37         Axman6: burst-sort? 
  18:38        edwardk: burst-sort is generally the fastest stringsort. and its built on the same sort of underpinnings as discrimination 
  18:38        edwardk: discrimination needs two things to really be fast 
  18:38        edwardk: and i just haven't done them 
  18:38        edwardk: 1.) i need a linear-probing hash table with simple tabulation hashing. the ioref holding a hashmap isn't cutting it 
  18:39        edwardk: and hashable is a terrible hash code 
  18:39        edwardk: simple tabulation hashing can be implemented as say a linked list of small hash tables for how to hash each byte in sequence that you xor the results of 
  18:39         Axman6: ah I';d also thought of using a trie to do this, glad I was on the right track 
  18:39        edwardk: and just build that hash table chain as a lazy list that gets threaded through the 'serializer' 
  18:40        edwardk: the second thing is this burst-trie stuff where we try to recover temporal coherence for cache access 
  18:40        edwardk: if on the other hand what you did was batch up in buckets until you got ~8192 things to distribute then you'd have coherence again for the distribution step 
  18:41        edwardk: this is what (the first version of) burst sort did 
  18:41        edwardk: so you'd now have two passes 
  18:41        edwardk: pass one would be dumping things into these buckets, which fill like water balloons, as you drip contents into them, bursting if they get too full, and building a larger and larger prefix of the trie 
  18:42        edwardk: then we take the trie that has leaf level buckets and walk that trie in order 
  18:42        edwardk: and for each bucket we can use a more specialized sort, e.g. multikey quicksort or something can be used locally 
  18:43        edwardk: this is the "right" version of discrimination 
  18:43         Axman6: I look forward to seeing it =) 
  18:43        edwardk: not anywhere near the top of my todo pile 
  18:43        edwardk: if you want it, build it ;) 
  18:43        edwardk: i'll happily help give pointers 
  18:44         Axman6: yeah I figured. I don't have anough need for it at the moment, but would be interested in building it if I could 
  18:44        edwardk: simple tabulation hashing + linear probing was proven basically optimal for this by mihai patrascu 
  18:45         Axman6: I feel like I've nerdsniped myself now 
  18:48        edwardk: =) 
  ```