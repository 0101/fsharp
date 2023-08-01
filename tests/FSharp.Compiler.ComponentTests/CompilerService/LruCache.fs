module CompilerService.LruCache
open Internal.Utilities.Collections

open Xunit
open System
open System.Threading
open System.Runtime.CompilerServices

[<Fact>]
let ``Adding an item to the cache should make it retrievable``() =
    let cache = new LruCache<int, string>(keepStrongly = 2)
    cache.Set(1, "one")
    let result = cache.TryGet(1)
    Assert.Equal("one", result.Value)

[<Fact>]
let ``Adding an item to the cache should evict the least recently used item if the cache is full``() =
    let cache = new LruCache<int, string>(keepStrongly = 2, keepWeakly = 0)
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(3, "three")
    let result = cache.TryGet(1)
    Assert.Null(result)

[<Fact>]
let ``Adding an item to the cache should not evict a required item``() =
    let cache = new LruCache<int, string>(keepStrongly = 2, requiredToKeep = (fun v -> v = "one"))
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(3, "three")
    let result = cache.TryGet(1)
    Assert.Equal("one", result.Value)

[<Fact>]
let ``Adding an item to the cache should not evict a strongly kept item``() =
    let cache = new LruCache<int, string>(keepStrongly = 2, keepWeakly = 0)
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(1, "one")
    cache.Set(3, "three")
    let result = cache.TryGet(1)
    Assert.Equal("one", result.Value)

[<Fact>]
let ``Adding an item to the cache should not evict a strongly kept item, even if it is the least recently used``() =
    let cache = new LruCache<int, string>(keepStrongly = 2, keepWeakly = 0, requiredToKeep = (fun v -> v = "one"))
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(3, "three")
    let result = cache.TryGet(1)
    Assert.Equal("one", result.Value)

[<Fact>]
let ``Adding an item to the cache should not evict a weakly kept item if its reference is still valid``() =
    let cache = new LruCache<int, string>(keepStrongly = 2, keepWeakly = 1)
    let value = "one"
    cache.Set(1, value)
    cache.Set(2, "two")
    GC.Collect(2, GCCollectionMode.Forced, true)
    let result = cache.TryGet(1)
    Assert.Equal(value, result.Value)


// Doing this directly in the test prevents GC for some reason
let private addObjToCache (cache: LruCache<_,_>) key =
    let o = obj ()
    cache.Set(key, o)

[<Fact>]
let ``Adding an item to the cache should evict a weakly kept item if its reference is no longer valid``() =
    let cache = new LruCache<_, _>(keepStrongly = 2, keepWeakly = 1)
    addObjToCache cache 1
    addObjToCache cache 2
    addObjToCache cache 3
    GC.Collect(2, GCCollectionMode.Forced, true)

    let result = cache.TryGet(1)
    Assert.Null(result)

[<Fact>]
let ``Moving a value from strongly to weakly kept and vice versa should work correctly``() =
    let cache = new LruCache<int, string>(keepStrongly = 2, keepWeakly = 2)
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(3, "three")
    let result1 = cache.TryGet(1)
    Assert.Equal("one", result1.Value)
    let result2 = cache.TryGet(2)
    Assert.Equal("two", result2.Value)
    let result3 = cache.TryGet(3)
    Assert.Equal("three", result3.Value)
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(3, "three")
    GC.Collect(2, GCCollectionMode.Forced, true)
    let result1 = cache.TryGet(1)
    Assert.Null(result1)
    let result2 = cache.TryGet(2)
    Assert.Equal("two", result2.Value)
    let result3 = cache.TryGet(3)
    Assert.Equal("three", result3.Value)
    cache.Set(2, "two")
    GC.Collect(2, GCCollectionMode.Forced, true)
    let result1 = cache.TryGet(1)
    Assert.Null(result1)
    let result2 = cache.TryGet(2)
    Assert.Equal("two", result2.Value)
    let result3 = cache.TryGet(3)
    Assert.Null(result3)
    cache.Set(1, "one")
    cache.Set(2, "two")
    cache.Set(3, "three")
    GC.Collect(2, GCCollectionMode.Forced, true)
    let result1 = cache.TryGet(1)
    Assert.Null(result1)
    let result2 = cache.TryGet(2)
    Assert.Equal("two", result2.Value)
    let result3 = cache.TryGet(3)
    Assert.Equal("three", result3.Value)