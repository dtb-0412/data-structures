# Public API

The following common parameter names are used throughout the descriptions below:  
- `value` has the same type as the container's `value_type`.
- `count` specifies the number of affected elements.
- `first` and `last` are iterators that delimit the range `[first, last)`. They may belong to the container or to an external source.
- `where` is an iterator that specifies the position at which an operation is performed and must belong to the container.
- `key` is a lookup key.
- `pred` is a function that takes one parameter of type `T` and returns `bool` to indicate whether that parameter satisfies a specific condition.  
  - Example signature:
    ```
    template<class T>
    bool is_even(const T& val) {
      return val % 2 == 0;
    }
    ```
- `comp` is a function that takes two parameters of type `T` and compares them according to an ordering relation.  
  - Example signature:
    ```
    template<class T>
    bool compare(const T& lhs, const T& rhs) {
      return lhs < rhs;
    }
    ```

## Sequence containers

### `DynamicArray<T>`

Contiguous dynamic storage with random-access iterators.

**Constructors and operators**

- `DynamicArray()`: construct an empty DynamicArray.
- `DynamicArray(count)`: construct `count` * value-initialized elements.
- `DynamicArray(count, const value&)`: construct `count` * `value`.
- `DynamicArray(first, last)`: construct range `[first, last)`.
- `DynamicArray(initList)`: construct from initList.
- `DynamicArray(const other&)`: copy-construct from `other`.
- `DynamicArray(other&&)`: move-construct from `other`.
- `~DynamicArray()`: destroy the DynamicArray and release its storage.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.
- `operator[](index)`: access the element at `index` without bounds checking.

**Accessors**

- `at(index)`: access the element at `index` with bounds checking.
- `begin()`: return an iterator to the first element.
- `end()`: return an iterator past the last element.
- `cbegin()`: return a const iterator to the first element.
- `cend()`: return a const iterator past the last element.
- `rbegin()`: return a reverse iterator to the last element.
- `rend()`: return a reverse iterator before the first element.
- `crbegin()`: return a const reverse iterator to the last element.
- `crend()`: return a const reverse iterator before the first element.
- `front()`: get the first element.
- `back()`: get the last element.
- `is_empty()`: check whether the DynamicArray is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.
- `capacity()`: get the current maximum number of elements before reallocation is needed.
- `unused_capacity()`: get the number of elements that can be added before reallocation is needed.
- `data()`: get a pointer to the internal contiguous array.

**Modifiers**

- `emplace(where, args&&...)`: insert by perfectly forwarding `args...` at `where`.
- `emplace_back(args&&...)`: insert by perfectly forwarding `args...` at back.
- `push_back(const value&)`: insert at back by copying `value`.
- `push_back(value&&)`: insert at back by moving `value`.
- `insert(where, count, const value&)`: insert `count` * `value` at `where`.
- `insert(where, const value&)`: insert by copying `value` at `where`.
- `insert(where, value&&)`: insert by moving `value` at `where`.
- `insert(where, first, last)`: insert range `[first, last)` at `where`.
- `insert(where, initList)`: insert initList at `where`.
- `append(count, const value&)`: append `count` * `value`.
- `append(first, last)`: append range `[first, last)`.
- `append(initList)`: append `initList`.
- `assign(count)`: assign `count` * value-initialized elements.
- `assign(count, const value&)`: assign `count` * `value`.
- `assign(first, last)`: assign range `[first, last)`.
- `assign(initList)`: assign initList.
- `pop_back()`: erase the last element.
- `erase(where)`: erase the element at `where`.
- `erase(first, last)`: erase range `[first, last)`.
- `clear()`: erase all elements and release the internal memory.
- `swap(other&)`: swap with `other`.
- `resize(newSize)`: trim or append value-initialized elements to reach `newSize`.
- `resize(newSize, const value&)`: trim or append copies of `value` to reach `newSize`.
- `reserve(newCapacity)`: expand capacity to `newCapacity`.
- `shrink_to_fit()`: shrink capacity to size.

· · ·

### `Deque<T>`

Block-based double-ended queue with random-access iterators.

**Constructors, operators, and accessors**

- `Deque()`: construct an empty Deque.
- `Deque(count)`: construct `count` * value-initialized elements.
- `Deque(count, const value&)`: construct `count` * `value`.
- `Deque(first, last)`: construct range `[first, last)`.
- `Deque(initList)`: construct from initList.
- `Deque(const other&)`: copy-construct from `other`.
- `Deque(other&&)`: move-construct from `other`.
- `~Deque()`: destroy the Deque and release its storage.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.
- `operator[](index)`: access the element at `index` without bounds checking.

**Accessors**

- `at(index)`: access the element at `index` with bounds checking.
- `begin()`: return an iterator to the first element.
- `end()`: return an iterator past the last element.
- `cbegin()`: return a const iterator to the first element.
- `cend()`: return a const iterator past the last element.
- `rbegin()`: return a reverse iterator to the last element.
- `rend()`: return a reverse iterator before the first element.
- `crbegin()`: return a const reverse iterator to the last element.
- `crend()`: return a const reverse iterator before the first element.
- `front()`: get the first element.
- `back()`: get the last element.
- `is_empty()`: check whether the Deque is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.
- `map_size()`: get the current number of block pointers in the internal map.
- `block_size()`: get the number of elements stored in each block.

**Modifiers**

- `emplace(where, args&&...)`: insert by perfectly forwarding `args...` at `where`.
- `emplace_front(args&&...)`: insert by perfectly forwarding `args...` at front.
- `emplace_back(args&&...)`: insert by perfectly forwarding `args...` at back.
- `push_front(const value&)`: insert at front by copying `value`.
- `push_front(value&&)`: insert at front by moving `value`.
- `push_back(const value&)`: insert at back by copying `value`.
- `push_back(value&&)`: insert at back by moving `value`.
- `insert(where, count, const value&)`: insert `count` * `value` at `where`.
- `insert(where, const value&)`: insert by copying `value` at `where`.
- `insert(where, value&&)`: insert by moving `value` at `where`.
- `insert(where, first, last)`: insert range `[first, last)` at `where`.
- `insert(where, initList)`: insert initList at `where`.
- `prepend(count, const value&)`: prepend `count` * `value`.
- `prepend(first, last)`: prepend range `[first, last)`.
- `prepend(initList)`: prepend initList.
- `append(count, const value&)`: append `count` * `value`.
- `append(first, last)`: append range `[first, last)`.
- `append(initList)`: append `initList`.
- `assign(count)`: assign `count` * value-initialized elements.
- `assign(count, const value&)`: assign `count` * `value`.
- `assign(first, last)`: assign range `[first, last)`.
- `assign(initList)`: assign initList.
- `pop_front()`: erase the first element.
- `pop_back()`: erase the last element.
- `erase(where)`: erase the element at `where`.
- `erase(first, last)`: erase range `[first, last)`.
- `clear()`: erase all elements and release the internal memory.
- `swap(other&)`: swap with `other`.
- `resize(newSize)`: trim or append value-initialized elements to reach `newSize`.
- `resize(newSize, const value&)`: trim or append copies of `value` to reach `newSize`.
- `shrink_to_fit()`: shrink the internal map and allocated blocks to the current size.

· · ·

### `List<T>`

Doubly linked list with bidirectional iterators.

**Constructors and operators**

- `List()`: construct an empty List.
- `List(count)`: construct `count` * value-initialized elements.
- `List(count, const value&)`: construct `count` * `value`.
- `List(first, last)`: construct range `[first, last)`.
- `List(initList)`: construct from initList.
- `List(const other&)`: copy-construct from `other`.
- `List(other&&)`: move-construct from `other`.
- `~List()`: destroy the List and release all nodes.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.

**Accessors**

- `begin()`: return an iterator to the first element.
- `end()`: return an iterator past the last element.
- `cbegin()`: return a const iterator to the first element.
- `cend()`: return a const iterator past the last element.
- `rbegin()`: return a reverse iterator to the last element.
- `rend()`: return a reverse iterator before the first element.
- `crbegin()`: return a const reverse iterator to the last element.
- `crend()`: return a const reverse iterator before the first element.
- `front()`: get the first element.
- `back()`: get the last element.
- `is_empty()`: check whether the List is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.

**Modifiers**

- `emplace(where, args&&...)`: insert by perfectly forwarding `args...` at `where`.
- `emplace_front(args&&...)`: insert by perfectly forwarding `args...` at front.
- `emplace_back(args&&...)`: insert by perfectly forwarding `args...` at back.
- `push_front(const value&)`: insert at front by copying `value`.
- `push_front(value&&)`: insert at front by moving `value`.
- `push_back(const value&)`: insert at back by copying `value`.
- `push_back(value&&)`: insert at back by moving `value`.
- `insert(where, count, const value&)`: insert `count` * `value` at `where`.
- `insert(where, const value&)`: insert by copying `value` at `where`.
- `insert(where, value&&)`: insert by moving `value` at `where`.
- `insert(where, first, last)`: insert range `[first, last)` at `where`.
- `insert(where, initList)`: insert initList at `where`.
- `prepend(count, const value&)`: prepend `count` * `value`.
- `prepend(first, last)`: prepend range `[first, last)`.
- `prepend(initList)`: prepend initList.
- `append(count, const value&)`: append `count` * `value`.
- `append(first, last)`: append range `[first, last)`.
- `append(initList)`: append `initList`.
- `assign(count)`: assign `count` * value-initialized elements.
- `assign(count, const value&)`: assign `count` * `value`.
- `assign(first, last)`: assign range `[first, last)`.
- `assign(initList)`: assign initList.
- `pop_front()`: erase the first element.
- `pop_back()`: erase the last element.
- `erase(where)`: erase the element at `where`.
- `erase(first, last)`: erase range `[first, last)`.
- `clear()`: erase all elements and release the internal memory except the sentinel.
- `swap(other&)`: swap with `other`.
- `resize(newSize)`: trim or append value-initialized elements to reach `newSize`.
- `resize(newSize, const value&)`: trim or append copies of `value` to reach `newSize`.
- `splice(where, other&)`: splice all of `other` into `*this` at `where`.
- `splice(where, other&&)`: splice all of `other` into `*this` at `where`.
- `splice(where, other&, first)`: splice the element at `first` from `other` into `*this` at `where`.
- `splice(where, other&&, first)`: splice the element at `first` from `other` into `*this` at `where`.
- `splice(where, other&, first, last)`: splice range `[first, last)` from `other` into `*this` at `where`.
- `splice(where, other&&, first, last)`: splice range `[first, last)` from `other` into `*this` at `where`.
- `remove(const value&)`: remove all elements matching `value`.
- `remove(const value&, first, last)`: remove all elements matching `value` in range `[first, last)`.
- `remove_if(pred)`: remove all elements satisfying `pred`.
- `remove_if(pred, first, last)`: remove all elements satisfying `pred` in range `[first, last)`.
- `remove_adjacent_if(pred)`: remove adjacent elements satisfying `pred`.
- `remove_adjacent_if(pred, first, last)`: remove adjacent elements satisfying `pred` in range `[first, last)`.
- `unique()`: remove all adjacent duplicates.
- `unique(first, last)`: remove all adjacent duplicates in range `[first, last)`.
- `reverse()`: reverse element order.
- `reverse(first, last)`: reverse element order in range `[first, last)`.
- `merge(other&)`: merge with `other` using `std::less<>`.
- `merge(other&&)`: merge with `other` using `std::less<>`.
- `merge(other&, comp)`: merge with `other` using `comp`.
- `merge(other&&, comp)`: merge with `other` using `comp`.
- `sort()`: sort elements using `std::less<>`.
- `sort(comp)`: sort elements using `comp`.
- `sort(first, last)`: sort range `[first, last)` using `std::less<>`.
- `sort(first, last, comp)`: sort range `[first, last)` using `comp`.

· · ·

### `ForwardList<T>`

Singly linked list with forward iterators.

**Constructors, operators**

- `ForwardList()`: construct an empty ForwardList.
- `ForwardList(count)`: construct `count` * value-initialized elements.
- `ForwardList(count, const value&)`: construct `count` * `value`.
- `ForwardList(first, last)`: construct range `[first, last)`.
- `ForwardList(initList)`: construct from initList.
- `ForwardList(const other&)`: copy-construct from `other`.
- `ForwardList(other&&)`: move-construct from `other`.
- `~ForwardList()`: destroy the ForwardList and release all nodes.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.

**Accessors**

- `before_begin()`: return an iterator before the first element.
- `cbefore_begin()`: return a const iterator before the first element.
- `begin()`: return an iterator to the first element.
- `end()`: return an iterator past the last element.
- `cbegin()`: return a const iterator to the first element.
- `cend()`: return a const iterator past the last element.
- `front()`: get the first element.
- `is_empty()`: check whether the ForwardList is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.

**Modifiers**

- `emplace_after(where, args&&...)`: insert after `where` by perfectly forwarding `args...`.
- `emplace_front(args&&...)`: insert by perfectly forwarding `args...` at front.
- `push_front(const value&)`: insert at front by copying `value`.
- `push_front(value&&)`: insert at front by moving `value`.
- `insert_after(where, count, const value&)`: insert `count` * `value` after `where`.
- `insert_after(where, const value&)`: insert by copying `value` after `where`.
- `insert_after(where, value&&)`: insert by moving `value` after `where`.
- `insert_after(where, first, last)`: insert range `[first, last)` after `where`.
- `insert_after(where, initList)`: insert initList after `where`.
- `prepend(count, const value&)`: prepend `count` * `value`.
- `prepend(first, last)`: prepend range `[first, last)`.
- `prepend(initList)`: prepend initList.
- `assign(count)`: assign `count` * value-initialized elements.
- `assign(count, const value&)`: assign `count` * `value`.
- `assign(first, last)`: assign range `[first, last)`.
- `assign(initList)`: assign initList.
- `pop_front()`: erase the first element.
- `erase_after(where)`: erase the element after `where`.
- `erase_after(first, last)`: erase range `(first, last)`.
- `clear()`: erase all elements and release the internal memory except the sentinel.
- `swap(other&)`: swap with `other`.
- `resize(newSize)`: trim or append value-initialized elements to reach `newSize`.
- `resize(newSize, const value&)`: trim or append copies of `value` to reach `newSize`.
- `splice_after(where, other&)`: splice all of `other` after `where`.
- `splice_after(where, other&&)`: splice all of `other` after `where`.
- `splice_after(where, other&, first)`: splice the element after `first` from `other` after `where`.
- `splice_after(where, other&&, first)`: splice the element after `first` from `other` after `where`.
- `splice_after(where, other&, first, last)`: splice range `(first, last)` from `other` after `where`.
- `splice_after(where, other&&, first, last)`: splice range `(first, last)` from `other` after `where`.
- `remove(const value&)`: remove all elements matching `value`.
- `remove_after(const value&, first, last)`: remove all elements matching `value` in range `(first, last)`.
- `remove_if(pred)`: remove all elements satisfying `pred`.
- `remove_if_after(pred, first, last)`: remove elements satisfying `pred` in range `(first, last)`.
- `remove_adjacent_if(pred)`: remove adjacent elements satisfying `pred`.
- `remove_adjacent_if_after(pred, first, last)`: remove adjacent elements satisfying `pred` in range `(first, last)`.
- `unique()`: remove all adjacent duplicates.
- `unique_after(first, last)`: remove all adjacent duplicates in range `(first, last)`.
- `reverse()`: reverse element order.
- `reverse_after(first, last)`: reverse element order in range `(first, last)`.
- `merge(other&)`: merge with `other` using `std::less<>`.
- `merge(other&&)`: merge with `other` using `std::less<>`.
- `merge(other&, comp)`: merge with `other` using `comp`.
- `merge(other&&, comp)`: merge with `other` using `comp`.
- `sort()`: sort elements using `std::less<>`.
- `sort(comp)`: sort elements using `comp`.
- `sort_after(first, last)`: sort range `(first, last)` using `std::less<>`.
- `sort_after(first, last, comp)`: sort range `(first, last)` using `comp`.

---

## Container adaptors

### `Stack<T, Cont = Deque<T>>`

LIFO container adaptor that inserts and removes elements from the top.

- `Stack()`: construct an empty Stack.
- `Stack(const container&)`: construct by copying from container.
- `Stack(container&&)`: construct by moving from container.
- `Stack(const other&)`: copy-construct from `other`.
- `Stack(other&&)`: move-construct from `other`.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `is_empty()`: check whether the Stack is empty.
- `size()`: get the current number of elements.
- `top()`: get the last element of the underlying container.
- `container()`: get the underlying container's const reference.
- `emplace(args&&...)`: construct an element in place at the top.
- `push(const value&)`: insert by copying `value` at top.
- `push(value&&)`: insert by moving `value` at top.
- `pop()`: remove the top element.
- `swap(other&)`: swap with `other`.

· · ·

### `Queue<T, Cont = Deque<T>>`

FIFO container adaptor that inserts elements at the back and removes them from the front.

- `Queue()`: construct an empty Queue.
- `Queue(const container&)`: construct by copying from container.
- `Queue(container&&)`: construct by moving from container.
- `Queue(const other&)`: copy-construct from `other`.
- `Queue(other&&)`: move-construct from `other`.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `is_empty()`: check whether the Queue is empty.
- `size()`: get the current number of elements.
- `front()`: get the first element.
- `back()`: get the last element.
- `container()`: get the underlying container's const reference.
- `emplace(args&&...)`: insert by perfectly forwarding `args...` at back.
- `push(const value&)`: insert by copying `value` at back.
- `push(value&&)`: insert by moving `value` at back
- `pop()`: remove the front element.
- `swap(other&)`: swap with `other`.

· · ·

### `PriorityQueue<T, Cont = DynamicArray<T>, Comp = std::less<...>>`

Heap-based container adaptor that keeps the highest-priority element at the top.

- `PriorityQueue()`: construct an empty PriorityQueue.
- `PriorityQueue(comp)`: construct an empty PriorityQueue with `comp`.
- `PriorityQueue(const container&, comp)`: construct by copying from container with `comp`.
- `PriorityQueue(container&&, comp)`: construct by moving from container with `comp`.
- `PriorityQueue(first, last)`: construct range `[first, last)`.
- `PriorityQueue(first, last, comp)`: construct range `[first, last)` with `comp`.
- `PriorityQueue(first, last, const container&, comp)`: construct by copying from container with `comp`, then insert  range `[first, last)`.
- `PriorityQueue(first, last, container&&, comp)`: construct by moving from container with `comp`, then insert  range `[first, last)`.
- `PriorityQueue(const other&)`: copy-construct from `other`.
- `PriorityQueue(other&&)`: move-construct from `other`.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `is_empty()`: check whether the PriorityQueue is empty.
- `size()`: get the current number of elements.
- `top()`: get the highest-priority element.
- `emplace(args&&...)`: insert by perfectly forwarding `args...` into the priority queue.
- `push(const value&)`: insert by copying `value` into the priority queue.
- `push(value&&)`: insert by moving `value` into the priority queue.
- `pop()`: remove the highest-priority element.
- `swap(other&)`: swap with `other`.

---

## Ordered associative containers

### `Set<T, Comp = std::less<T>>` and `MultiSet<T, Comp = std::less<T>>`

Ordered associative container that stores unique or non-unique keys.

**Constructors and operators**
- `Set()`, `MultiSet()`: construct an empty Set or MultiSet.
- `Set(comp)`, `MultiSet(comp)`: construct an empty Set or MultiSet with `comp`.
- `Set(first, last)`, `MultiSet(first, last)`: construct range `[first, last)`.
- `Set(first, last, comp)`, `MultiSet(first, last, comp)`: construct range `[first, last)` with `comp`.
- `Set(initList)`, `MultiSet(initList)`: construct from initList.
- `Set(initList, comp)`, `MultiSet(initList, comp)`: construct from initList with `comp`.
- `Set(const other&)`, `MultiSet(const other&)`: copy-construct from `other`.
- `Set(other&&)`, `MultiSet(other&&)`: move-construct from `other`.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.

**Accessors**
- `begin()`: return an iterator to the smallest element.
- `end()`: return an iterator past the largest element.
- `cbegin()`: return a const iterator to the smallest element.
- `cend()`: return a const iterator past the largest element.
- `rbegin()`: return a reverse iterator to the largest element.
- `rend()`: return a reverse iterator before the smallest element.
- `crbegin()`: return a const reverse iterator to the largest element.
- `crend()`: return a const reverse iterator before the smallest element.
- `min()`: get the smallest element.
- `max()`: get the largest element.
- `is_empty()`: check whether the Set or MultiSet is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.
- `key_comp()`: get the key comparator.
- `value_comp()`: get the value comparator.

**Modifiers**
- `emplace(args&&...)`: insert by perfectly forwarding `args...`.
- `emplace_hint(hint, args&&...)`: insert by perfectly forwarding `args...` with `hint`.
- `insert(const value&)`: insert by copying `value`.
- `insert(value&&)`: insert by moving `value`.
- `insert(hint, const value&)`: insert by copying `value` with `hint`.
- `insert(hint, value&&)`: insert by moving `value` with `hint`.
- `insert(first, last)`: insert range `[first, last)`.
- `insert(initList)`: insert initList.
- `insert(handle&&)`: insert the element owned by `handle`.
- `insert(hint, handle&&)`: insert the element owned by `handle` with `hint`.
- `erase(where)`: erase the element at `where`.
- `erase(first, last)`: erase range `[first, last)`.
- `erase(const key&)`: erase all elements matching or equivalent to `key`.
- `clear()`: erase all elements and release their nodes.
- `swap(other&)`: swap with `other`.
- `merge(other&)`: merge with `other` using `key_compare`.
- `merge(other&&)`: merge with `other` using `key_compare`.
- `find(const key&)`: find the first element matching or equivalent to `key`.
- `contains(const key&)`: check whether matching or equivalent to `key` exists.
- `count(const key&)`: count elements matching or equivalent to `key`.
- `lower_bound(const key&)`: find the first element not less than `key`.
- `upper_bound(const key&)`: find the first element greater than `key`.
- `equal_range(const key&)`: find the range of elements matching or equivalent to `key`.
- `extract(where)`: extract the element at `where` into a node handle.
- `extract(const key&)`: extract the element matching `key` into a node handle.

· · ·

### `Map<Key, T, Comp = std::less<Key>>` and `MultiMap<Key, T, Comp = std::less<Key>>`

Ordered associative container that stores unique or non-unique key-value pairs.

**Constructors and operators**
- `Map()`, `MultiMap()`: construct an empty Map or MultiMap.
- `Map(comp)`, `MultiMap(comp)`: construct an empty Map or MultiMap with `comp`.
- `Map(first, last)`, `MultiMap(first, last)`: construct range `[first, last)`.
- `Map(first, last, comp)`, `MultiMap(first, last, comp)`: construct range `[first, last)` with `comp`.
- `Map(initList)`, `MultiMap(initList)`: construct from initList.
- `Map(initList, comp)`, `MultiMap(initList, comp)`: construct from initList with `comp`.
- `Map(const other&)`, `MultiMap(const other&)`: copy-construct from `other`.
- `Map(other&&)`, `MultiMap(other&&)`: move-construct from `other`.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.

**Accessors**
- `at(key)`: access the value mapped by `key` with key checking.
- `begin()`: return an iterator to the smallest element.
- `end()`: return an iterator past the largest element.
- `cbegin()`: return a const iterator to the smallest element.
- `cend()`: return a const iterator past the largest element.
- `rbegin()`: return a reverse iterator to the largest element.
- `rend()`: return a reverse iterator before the smallest element.
- `crbegin()`: return a const reverse iterator to the largest element.
- `crend()`: return a const reverse iterator before the smallest element.
- `min()`: get the smallest element.
- `max()`: get the largest element.
- `is_empty()`: check whether the Map or MultiMap is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.
- `key_comp()`: get the key comparator.
- `value_comp()`: get the value comparator.

**Modifiers**
- `emplace(args&&...)`: insert by perfectly forwarding `args...`.
- `emplace_hint(hint, args&&...)`: insert by perfectly forwarding `args...` with `hint`.
- `insert(const value&)`: insert by copying `value`.
- `insert(value&&)`: insert by moving `value`.
- `insert(hint, const value&)`: insert by copying `value` with `hint`.
- `insert(hint, value&&)`: insert by moving `value` with `hint`.
- `insert(first, last)`: insert range `[first, last)`.
- `insert(initList)`: insert initList.
- `insert(handle&&)`: insert the element owned by `handle`.
- `insert(hint, handle&&)`: insert the element owned by `handle` with `hint`.
- `erase(where)`: erase the element at `where`.
- `erase(first, last)`: erase range `[first, last)`.
- `erase(const key&)`: erase all elements matching or equivalent to `key`.
- `clear()`: erase all elements and release their nodes.
- `swap(other&)`: swap with `other`.
- `merge(other&)`: merge with `other` using `key_compare`.
- `merge(other&&)`: merge with `other` using `key_compare`.
- `find(const key&)`: find the first element matching or equivalent to `key`.
- `contains(const key&)`: check whether an element matching or equivalent to `key` exists.
- `count(const key&)`: count elements matching or equivalent to `key`.
- `lower_bound(const key&)`: find the first element not less than `key`.
- `upper_bound(const key&)`: find the first element greater than `key`.
- `equal_range(const key&)`: find the range of elements matching or equivalent to `key`.
- `extract(where)`: extract the element at `where` into a node handle.
- `extract(const key&)`: extract the element matching `key` into a node handle.

· · ·

### Map exclusive:
- `operator[](const key&)`: access the value mapped by `key`. If the mapped value is absent, insert a default-initialized value for `key`.
- `try_emplace(const key&, args&&...)`: if the value mapped by `key` is absent, insert key-value pair by copying from `key` and perfectly forwarding `args...`.
- `try_emplace(key&&, args&&...)`: if the value mapped by `key` is absent, insert key-value pair by moving from `key` and perfectly forwarding `args...`.
- `try_emplace_hint(hint, const key&, args&&...)`: if the value mapped by `key` is absent, insert key-value pair by copying from `key` and perfectly forwarding `args...` with `hint`.
- `try_emplace_hint(hint, key&&, args&&...)`: if the value mapped by `key` is absent, insert key-value pair by moving from `key` and perfectly forwarding `args...` with `hint`.
- `insert_or_assign(const key&, const value&)`: if the value mapped by `key` is absent, insert key-value pair by copying from `key` and perfectly forwarding `args...`. Otherwise, assign `value` to the value mapped by `key`.
- `insert_or_assign(key&&, value&&)`: if the value mapped by `key` is absent, insert key-value pair by moving from `key` and perfectly forwarding `args...`. Otherwise, assign `value` to the value mapped by `key`.
- `insert_or_assign_hint(hint, const key&, const value&)`: if the value mapped by `key` is absent, insert key-value pair by copying from `key` and perfectly forwarding `args...` with `hint`. Otherwise, assign `value` to the value mapped by `key`.
- `insert_or_assign_hint(hint, key&&, value&&)`: if the value mapped by `key` is absent, insert key-value pair by moving from `key` and perfectly forwarding `args...` with `hint`. Otherwise, assign `value` to the value mapped by `key`.

· · ·

### Multimap exclusive:
- `emplace(args&&...)`: insert key-value pair by perfectly forwarding `args...`.
- `insert(value&&)`: insert key-value pair by moving `value`.
- `insert(hint, value&&)`: insert key-value pair by moving `value` with `hint`.


---

## Tree containers

The following tree types expose the same ordered-associative API, with the tree implementation reflected in their public type:
- `AVLTree<T, Comp = std::less<T>>`: unique key AVL tree.
- `AVLMultiTree<T, Comp = std::less<T>>`: non-unique key AVL tree.
- `RedBlackTree<T, Comp = std::less<T>>`: unique key red-black tree.
- `RedBlackMultiTree<T, Comp = std::less<T>>`: non-unique key red-black tree.

**Constructors and operators**
- `Tree()`: construct an empty tree.
- `Tree(comp)`: construct an empty Tree with `comp`.
- `Tree(first, last)`: construct range `[first, last)`.
- `Tree(first, last, comp)`: construct range `[first, last)` with `comp`.
- `Tree(initList)`: construct from initList.
- `Tree(initList, comp)`: construct from initList with `comp`.
- `Tree(const other&)`: copy-construct from `other`.
- `Tree(other&&)`: move-construct from `other`.
- `operator=(const other&)`: copy-assign from `other`.
- `operator=(other&&)`: move-assign from `other`.
- `operator=(initList)`: assign initList.

**Accessors**
- `begin()`: return an iterator to the smallest element.
- `end()`: return an iterator past the largest element.
- `cbegin()`: return a const iterator to the smallest element.
- `cend()`: return a const iterator past the largest element.
- `rbegin()`: return a reverse iterator to the largest element.
- `rend()`: return a reverse iterator before the smallest element.
- `crbegin()`: return a const reverse iterator to the largest element.
- `crend()`: return a const reverse iterator before the smallest element.
- `min()`: get the smallest element.
- `max()`: get the largest element.
- `is_empty()`: check whether the Tree is empty.
- `size()`: get the current number of elements.
- `max_size()`: get the maximum possible number of elements.
- `key_comp()`: get the key comparator.
- `value_comp()`: get the value comparator.

**Modifiers**
- `emplace(args&&...)`: insert by perfectly forwarding `args...`.
- `emplace_hint(hint, args&&...)`: insert by perfectly forwarding `args...` with `hint`.
- `insert(const value&)`: insert by copying `value`.
- `insert(value&&)`: insert by moving `value`.
- `insert(hint, const value&)`: insert by copying `value` with `hint`.
- `insert(hint, value&&)`: insert by moving `value` with `hint`.
- `insert(first, last)`: insert range `[first, last)`.
- `insert(initList)`: insert initList.
- `insert(handle&&)`: insert the element owned by `handle`.
- `insert(hint, handle&&)`: insert the element owned by `handle` with `hint`.
- `erase(where)`: erase the element at `where`.
- `erase(first, last)`: erase range `[first, last)`.
- `erase(const key&)`: erase all elements matching or equivalent to `key`.
- `clear()`: erase all elements and release their nodes.
- `swap(other&)`: swap with `other`.
- `merge(other&)`: merge with `other` using `key_compare`.
- `merge(other&&)`: merge with `other` using `key_compare`.
- `find(const key&)`: find the first element matching or equivalent to `key`.
- `contains(const key&)`: check whether an element matching or equivalent to `key` exists.
- `count(const key&)`: count elements matching or equivalent to `key`.
- `lower_bound(const key&)`: find the first element not less than `key`.
- `upper_bound(const key&)`: find the first element greater than `key`.
- `equal_range(const key&)`: find the range of elements matching or equivalent to `key`.
- `extract(where)`: extract the element at `where` into a node handle.
- `extract(const key&)`: extract the element matching `key` into a node handle.
