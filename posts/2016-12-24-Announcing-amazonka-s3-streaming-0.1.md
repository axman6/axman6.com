---
title: Announcing amazonka-s3-streaming-0.1
snippet: Announcing amazonka-s3-streaming-0.1, for efficient streaming and concurrent upload to S3.
author: Alex Mason
ident: c525c6fe-96ec-4c2e-b038-105a3638c9ea 
image: Storage-Content-Delivery_AmazonS3.svg
image-attr: Amazon
---

I've spent some time over the last week writing
[amazonka-s3-streaming](https://hackage.haskell.org/package/amazonka-s3-streaming)
(<a href="https://github.com/Axman6/amazonka-s3-streaming"><i class="fa fa-github" aria-hidden="true"></i></a>) which 
provides a conduit based streaming interface for uploading files/data to Amazon'a S3 service.
There's also a way to upload files or strict ByteStrings to S3 concurrently using the async package.
The package is based on Brendan Hay's amazing [Amazonka](https://hackage.haskell.org/package/amazonka)
(<a href="https://github.com/brendanhay/amazonka"><i class="fa fa-github" aria-hidden="true"></i></a>)
suite of libraries.

I've tried to make things as efficient as possible, and to avoid excess memory usage if I can.
A few of the ways I've attempted to do this are: 

## `mmap`ing file segments

when concurrently uploading a file, each file part (currently 6MiB) is
read in using `mmap`. The ensures that the mappings can be garbage collected as soon as they're
sent, usually resulting in much lower memory usage that mapping the whole file might.

The one big caveat here, which is unlikely to affect many people, but worth keeping in mind is
that mmapping ByteStrings can break referential transparancy - if another process modifies the
file, the contents of these parks might change.

## Avoiding `ByteString.Builder`

Initially I was accumulating data in the conduit streaming
interface using a ByteString `Builder`, and then converting this to a Lazy ByteString for
upload. It occured to me that
    
  a) the bytestrings are already in memory
  b) by converting them to Builders, buffers would be allocated to produce the
      Lazy ByteString
  c) these buffers would be immediately written to the socket
  d) so why not just use the already allocated ByteStrings and send them to the socket.

so I changed the implementation to store a (D)List of ByteStrings until more than 6MiB have
been accumulated, and this list is then turned into a conduit `Source` to be sent by the
`UploadPart` request to S3. I also keep track of the length and the SHA256 hash and total
length, which are needed by `amazonka`.

The Builder based implementation was my initial experiment, and didn't survive long enough
compare the performance with the list based implementation, so I don't have benchmarks to
prove this was a sensible move or not. 

My experiments so far have shown that the concurrent upload is capable of saturating a 1Gbps
connection quite easily, while using ~70MB RAM uploading a multi-hundred-megabyte file. The
streaming interface has managed up to 40MB/s in my testing in GHCi. I'll update this post once
I'm back at work in the new year with some numbers.

I was inspired by [a question](https://github.com/brendanhay/amazonka/issues/343) by Alex Babkin
about how to stream data into S3. His usecase is pretty cool, wanting to use his
<a href="https://github.com/ababkin/qmuli">Qmuli <i class="fa fa-github" aria-hidden="true"></i></a>
package to stream data through AWS Lambdas written in Haskell without needing to download the entire
file. Hopefully this package will allow this, while using no more than a few tens of MB more RAM
than you would expect if you were able to stream the data directly over the network.

I'd love any feedback you've got, either here or on 
<a href="https://www.reddit.com/r/haskell/comments/5k34gg/announcing_amazonkas3streaming01_efficient/">Reddit <i class="fa fa-reddit" aria-hidden="true"></i></a>.