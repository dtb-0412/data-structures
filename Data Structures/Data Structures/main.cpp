#include"tests.h"

int main() {
	//avl_tree_test();
	//deque_test();
	//dynamic_array_test();
	forward_list_test();
	//list_test();
	//map_test();
	//rb_tree_test();
	//set_test();
	return 0;
}

/*
Public API:

* Unordered internal structure
Constructors & destructor:
	Ctor(): Construct empty object

		// Requires linear internal structure (All but Unordered(Multi)Map, Unordered(Multi)Set)
	Ctor(count): Construct count * value-initialized
	Ctor(count, const& val): Construct count * val

	Ctor(first, last): Construct range [first, last)
	Ctor(initList): Construct range [initList.begin(), initList.end())
	Ctor(const&): Copy construct
	Ctor(&&): Move construct
	Dtor(): Destruct

Operator:
	operator=(const&): Copy assign
	operator=(&&): Move assign
	operator=(initList): Assign range [initList.begin(), initList.end())

		// Requires random access iterator (DynamicArray, Deque)
	operator[](index)/operator[](index) const: get element at index

		// Requires key-value structure (Unordered(Multi)Map)
	operator[](key)/operator[](key) const: get element referenced by key

Accessor:
	begin()/begin() const: begin iterator
	end()/end() const: end iterator
	cbegin(): const begin iterator
	cend(): const end iterator

		// Requires bidirectional iterator (All but ForwardList)
	rbegin()/rbegin() const: begin reverse iterator
	rend()/rend() const: end reverse iterator
	crbegin(): const begin reverse iterator
	crend(): const end reverse iterator

		// Requires key-value structure (Unordered(Multi)Map)
	at(key)/at(key) const: get element referenced by key
		
		// Requires O(1) front operations (ForwardList, List, Deque)
	front()/front() const: get first element

		// Requires O(1) back operations (List, Deque, DynamicArray)
	back()/back() const: get last element

	is_empty(): check if container is empty
	size(): get current number of elements
	max_size(): get maximum number of elements

		// Requires random access iterator (DynamicArray, Deque)
	at(index)/at(index) const: get element at index

		// Requires pre-allocated dynamic memory (DynamicArray, Deque)
	capacity(): get current maximum number of elements before reallocation is needed

		// DynamicArray specific
	data()/const data(): get internal array pointer

Modifier:
		// Requires O(1) front operations
	push_front(const& val)/push_front(&& val): insert at beginning by copying/moving val
	emplace_front(&&... args): insert at beginning by constructing args in place
	pop_front(): erase first element
	prepend(first, last): insert range [first, last) at beginning
		// Requires non-unique internal structure (All but UnorderedMap)
	prepend(count): insert count * value-initialized at beginning
	prepend(count, const& val): insert count * val at beginning

		// Requires O(1) back operations
	push_back(const& val)/push_back(&& val): insert at end by copying/moving val
	emplace_back(&&... args): insert at end by constructing args in place
	pop_back(): erase last element
	append(first, last): insert range [first, last) at end
		// Requires non-unique internal structure
	append(count): insert count * value-initialized at end
	append(count, const& val): insert count * val at end

		// Requires linear internal structure (All but Unordered(Multi)Map, Unordered(Multi)Set)
	insert(where, count): insert count * value-initialized at where
	insert(where, count, const& val): insert count * val at where

	emplace(where, &&... args): insert at where by constructing args in place
	insert(where, const& val)/insert(where, && val): insert at where by copying/moving val
	insert(where, first, last): insert range [first, last) at where
	insert(where, initList): insert range [initList.begin(), initList.end()) at where
	
	assign(count, const& val): assign count * val
	assign(where, first, last): assign range [first, last) at where
	assign(where, initList): assign range [initList.begin(), initList.end()) at where
	erase(where): erase element at where
	erase(first, last): erase range [first, last)
	
	clear(): erase all elements, free all internal memory except sentinel
	swap(& other): swap with other
	
	resize(newSize): trim or append value-initialized elements to reach newSize
	resize(newSize, const& val): trim or append copies of val to reach newSize

		// Requires contiguous block-based internal structure (DynamicArray, Deque)
	reserve(newCapacity): Expand capacity to at least newCapacity
	shrink_to_fit(): Shrink capacity to at least current size

		// Requires node-based internal structure (ForwardList, List)
	splice(where, & other)/splice((where, && other): splice all of other into *this at where
	splice(where, & other, first)/splice(where, && other, first): splice one node at first from other into *this at where
	splice(where, & other, first, last)/splice(where, && other, first, last): splice range [first, last) from other into *this at where
	
	remove(conts& val): remove all elements matching val in *this
	remove(const& val, first, last): remove all elements matching val in range [first, last)
	remove_if(pred): remove all elements matching pred in *this
	remove_if(pred, first, last): remove all elements matching pred in range [first, last)
	remove_adjacent_if(pred): remove all adjacent elements matching pred in *this
	remove_adjacent_if(pred, first, last): remove all adjacent elements matching pred in range [first, last)
	
	unique: remove all adjacent duplicates in *this
	unique(first, last): remove all adjacent duplicates in range [first, last)
	reverse(): reverse elements order in *this
	reverse(first, last): reverse elements order in range [first, last)
	
	merge(& other)/merge(&& other): merge with other, both *this and other must be sorted using std::less<>
	merge(& other, comp)/merge(&& other, comp): merge with other, both *this and other must be sorted using comp
	sort()/sort(comp): sort *this using std::less<>/comp

		// ForwardList specific (with *_after suffix)
	emplace_after(where, &&... args): insert after where by constructing args in place
	insert_after(where, const& val)/insert_after(where, && val): insert after where by copying/moving val
	insert_after(where, count): insert count * value-initialized after where
	insert_after(where, count, const& val): insert count * val after where
	insert_after(where, first, last): insert range [first, last) after where
	insert_after(where, initList): insert range [initList.begin(), initList.end()) after where
	
	erase_after(where): erase element after where
	erase_after(first, last): erase range (first, last)
	
	splice_after(where, & other)/splice_after(where, && other): splice all of other into *this after where
	splice_after(where, & other, first)/splice_after(where, && other, first): splice one node at first from other into *this after where
	splice_after(where, & other, first, last)/splice_after(where, && other, first, last): splice range [first, last) from other into *this after where
	
	remove_after(const& val, first, last): remove all elements matching val in range (first, last)
	remove_if_after(pred, first, last): remove all elements matching pred in range (first, last)
	remove_adjacent_if_after(pred, first, last): remove all adjacent elements matching pred in range (first, last)
	
	unique_after(first, last): remove all adjacent duplicates in range (first, last)
	reverse_after(first, last): reverse elements order in range (first, last)
	
* Ordered internal structure
Constructors & destructor:
	Ctor(): Construct empty object
	Ctor(first, last): Construct range [first, last)
	Ctor(initList): Construct range [initList.begin(), initList.end())
	Ctor(const&): Copy construct
	Ctor(&&): Move construct
	Dtor(): Destruct

Operator:
	operator=(const&): Copy assign
	operator=(&&): Move assign
	operator=(initList): Assign range [initList.begin(), initList.end())

		// Requires key-value structure
	operator[](key)/operator[](key) const: get element referenced by key

Accessor:
	begin()/begin() const: begin iterator
	end()/end() const: end iterator
	cbegin(): const begin iterator
	cend(): const end iterator

	rbegin()/rbegin() const: begin reverse iterator
	rend()/rend() const: end reverse iterator
	crbegin(): const begin reverse iterator
	crend(): const end reverse iterator

	is_empty(): check if container is empty
	size(): get current number of elements
	max_size(): get maximum number of elements

	min()/min() const: get smallest element
	max()/max() const: get largest element
	
	key_comp()/key_comp() const: get key comparator
	value_comp()/value_comp() const: get value comparator

Modifier:
	emplace(&&... args): insert by constructing args in place
	emplace_hint(hint, &&... args): insert with hint by constructing args in place
	insert(const& val)/insert(w&& val): insert by copying/moving val
	insert(hint, const& val)/insert(hint, && val): insert with hint by copying/moving val
	insert(where, first, last): insert range [first, last)
	insert(initList): insert range [initList.begin(), initList.end())
	insert(handle): insert node handle
	insert(hint handle): insert node handle with hint

	erase(where): erase element at where
	erase(first, last): erase range [first, last)
	erase(const& key): erase key
	erase(<>&& key): erase equivalent to key
	
	clear(): erase all elements, free all internal memory except sentinel
	swap(& other): swap with other

	merge(& other)/merge(&& other): merge with other

	extract(key): extract key
	extract(where): extract element at where

Query:
	find(const& key)/find(const& key) const: find key
	find(const<>& key)/find(const<>& key) const: find equivalent to key

	contains(const& key): check if key exists
	contains(const<>& key): check if equivalent to key exists

	count(const& key): count occurrence of key
	count(const<>& key): count occurrence of equivalent to key

	lower_bound(const& key)/lower_bound(const& key) const: find lower bound from key
	lower_bound(const<>& key)/lower_bound(const<>& key) const: find lower bound from equivalent to key

	upper_bound(const& key)/upper_bound(const& key) const: find upper bound from key
	upper_bound(const<>& key)/upper_bound(const<>& key) const: find upper bound from equivalent to key

	equal_range(const& key)/equal_range(const& key) const: find range of elements equal to key
	equal_range(const<>& key)/equal_range(const<>& key) const: find range of elements equivalent to key
*/