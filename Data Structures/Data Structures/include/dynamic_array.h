#pragma once
#ifndef DYNAMIC_ARRAY_H
#define DYNAMIC_ARRAY_H

#include"compare.hpp"
#include"memory.hpp"
#include"type_traits.hpp"

template<class DynamicArrVal>
class _DynamicArrayConstIterator {
private:
	using _Pointer = typename DynamicArrVal::pointer;

public:
	using iterator_concept	= std::contiguous_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DynamicArrVal::value_type;
	using difference_type	= typename DynamicArrVal::difference_type;
	using pointer			= typename DynamicArrVal::pointer;
	using reference			= const value_type&;

	constexpr _DynamicArrayConstIterator() noexcept
		: ptr() {}

	constexpr _DynamicArrayConstIterator(_Pointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] constexpr reference operator*() const noexcept {
		return *ptr; // UB: nullptr or end() dereference
	}

	[[nodiscard]] constexpr pointer operator->() const noexcept {
		return ptr;
	}

	constexpr _DynamicArrayConstIterator& operator++() noexcept {
		++ptr;  // UB: increment past end()
		return *this;
	}

	constexpr _DynamicArrayConstIterator operator++(int) noexcept {
		_DynamicArrayConstIterator temp = *this;
		++(*this);
		return temp;
	}

	constexpr _DynamicArrayConstIterator& operator--() noexcept {
		--ptr;  // UB: decrement past begin()
		return *this;
	}

	constexpr _DynamicArrayConstIterator operator--(int) noexcept {
		_DynamicArrayConstIterator temp = *this;
		--(*this);
		return temp;
	}

	constexpr _DynamicArrayConstIterator& operator+=(const difference_type offset) noexcept {
		ptr += offset;  // UB: increment past end()
		return *this;
	}

	[[nodiscard]] constexpr _DynamicArrayConstIterator operator+(const difference_type offset) const noexcept {
		_DynamicArrayConstIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] friend constexpr _DynamicArrayConstIterator operator+(const difference_type offset, _DynamicArrayConstIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	constexpr _DynamicArrayConstIterator& operator-=(const difference_type offset) noexcept {
		ptr -= offset;
		return *this;
	}

	[[nodiscard]] constexpr _DynamicArrayConstIterator operator-(const difference_type offset) const noexcept {
		_DynamicArrayConstIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] constexpr difference_type operator-(const _DynamicArrayConstIterator& other) const noexcept {
		return static_cast<difference_type>(ptr - other.ptr);  // UB: 2 iterators don't belong to the same container
	}

	[[nodiscard]] constexpr reference operator[](const difference_type offset) const noexcept {
		return *(*this + offset);  // UB: nullptr or end() dereference, offset out of range
	}

	[[nodiscard]] constexpr bool operator==(const _DynamicArrayConstIterator& other) const noexcept {
		return ptr == other.ptr;  // UB: iterators don't belong to the same container
	}

	[[nodiscard]] constexpr std::strong_ordering operator<=>(const _DynamicArrayConstIterator& other) const noexcept {
		return ptr <=> other.ptr;
	}

public:
	_Pointer ptr;
};

template<class DynamicArrVal>
class _DynamicArrayIterator : public _DynamicArrayConstIterator<DynamicArrVal> {
private:
	using _BaseIter = _DynamicArrayConstIterator<DynamicArrVal>;
	using _BaseIter::_BaseIter;  // Inherit _BaseIter's constructors

public:
	using iterator_concept	= std::contiguous_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DynamicArrVal::value_type;
	using difference_type	= typename DynamicArrVal::difference_type;
	using pointer			= typename DynamicArrVal::pointer;
	using reference			= value_type&;

	[[nodiscard]] constexpr reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] constexpr pointer operator->() const noexcept {
		return this->ptr;
	}

	constexpr _DynamicArrayIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	constexpr _DynamicArrayIterator operator++(int) noexcept {
		_DynamicArrayIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	constexpr _DynamicArrayIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	constexpr _DynamicArrayIterator operator--(int) noexcept {
		_DynamicArrayIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}

	constexpr _DynamicArrayIterator& operator+=(const difference_type offset) noexcept {
		_BaseIter::operator+=(offset);
		return *this;
	}

	[[nodiscard]] constexpr _DynamicArrayIterator operator+(const difference_type offset) const noexcept {
		_DynamicArrayIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] constexpr friend _DynamicArrayIterator operator+(const difference_type offset, _DynamicArrayIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	constexpr _DynamicArrayIterator& operator-=(const difference_type offset) noexcept {
		_BaseIter::operator-=(offset);
		return *this;
	}

	// There are 2 overloads of _BaseIter::operator-(). If we override any one of them, the others will be hidden by default.
	// In this case, only the operator-(const difference_type) needs overriding.
	// Therefore we explicitly tell the compiler to inherit all other overloads except for the ones being overridden.
	using _BaseIter::operator-;

	[[nodiscard]] constexpr _DynamicArrayIterator operator-(const difference_type offset) const noexcept {
		_DynamicArrayIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] constexpr reference operator[](const difference_type offset) const noexcept {
		return const_cast<reference>(_BaseIter::operator[](offset));
	}
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr>
struct _DynamicArrValue {
	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;

	constexpr _DynamicArrValue() noexcept
		: first(), last(), end() {}

	constexpr _DynamicArrValue(pointer first, pointer last, pointer end) noexcept
		: first(first), last(last), end(end) {}

	constexpr void clear() noexcept {
		if (first) {
			memory::destruct(first, last);
			memory::deallocate(first, static_cast<size_type>(end - first) * sizeof(value_type));

			first	= nullptr;
			last	= nullptr;
			end		= nullptr;
		}
	}

	constexpr void swap(_DynamicArrValue& other) noexcept {
		using std::swap; // Intentional ADL
		swap(first, other.first);
		swap(last, other.last);
		swap(end, other.end);
	}

	pointer first;	// Points to the first element in the array
	pointer last;	// Points to past-the-end element in the array (size)
	pointer end;	// Points to the end of allocated memory in the array (capacity)
};

struct _ValueInitializeTag {
	explicit _ValueInitializeTag() = default;
};

template<class DynamicArrVal>
struct _ArrayConstructGuard {
	// Guard for array construction failure
	_ArrayConstructGuard(DynamicArrVal* ptr)
		: ptr(ptr) {}

	_ArrayConstructGuard(const _ArrayConstructGuard&)				= delete;
	_ArrayConstructGuard& operator=(const _ArrayConstructGuard&)	= delete;

	constexpr ~_ArrayConstructGuard() noexcept {
		if (ptr) {
			ptr->clear();
		}
	}

	constexpr void release() noexcept {
		ptr = nullptr;
	}

	DynamicArrVal* ptr;
};

template<class DynamicArrVal>
struct _ArrayReallocateGuard {
	// Guard for array reallocation failure
	using value_type	= typename DynamicArrVal::value_type;
	using size_type		= typename DynamicArrVal::size_type;
	using pointer		= typename DynamicArrVal::pointer;

	_ArrayReallocateGuard(const size_type newCapacity, pointer newFirst)
		: newCapacity(newCapacity), newFirst(newFirst) {}

	_ArrayReallocateGuard(const _ArrayReallocateGuard&)				= delete;
	_ArrayReallocateGuard& operator=(const _ArrayReallocateGuard&)	= delete;

	constexpr ~_ArrayReallocateGuard() noexcept {
		if (newFirst) {
			memory::deallocate(newFirst, newCapacity * sizeof(value_type));
		}
	}

	constexpr void release() noexcept {
		newFirst = nullptr;
	}

	size_type	newCapacity;	// Capacity of the new array
	pointer		newFirst;		// Points to the new array
};

template<class DynamicArrVal>
struct _ArrayTransferGuard {
	// Guard for array transfer failure when inserting or resizing
	using size_type = typename DynamicArrVal::size_type;
	using pointer	= typename DynamicArrVal::pointer;

	_ArrayTransferGuard(const size_type newCapacity, pointer newFirst, pointer constructedFirst, pointer constructedLast)
		: base(newCapacity, newFirst), constructedFirst(constructedFirst), constructedLast(constructedLast) {}

	_ArrayTransferGuard(const _ArrayTransferGuard&)				= delete;
	_ArrayTransferGuard& operator=(const _ArrayTransferGuard&)	= delete;

	constexpr ~_ArrayTransferGuard() noexcept {
		if (constructedFirst) {
			memory::destruct(constructedFirst, constructedLast);
		}
	}

	constexpr void release() noexcept {
		constructedFirst	= nullptr;
		constructedLast		= nullptr;
		base.release();
	}

	_ArrayReallocateGuard<DynamicArrVal> base;
	pointer constructedFirst;	// Start of the constructed range
	pointer constructedLast;	// One-past-end of the constructed range
};

template<class DynamicArrVal>
struct _ArrayVaporizeGuard {
	// Guard for double failure when inserting range of elements
	using pointer = typename DynamicArrVal::pointer;

	_ArrayVaporizeGuard(DynamicArrVal* ptr, pointer vaporizedFirst, pointer destructedFirst)
		: ptr(ptr), vaporizedFirst(vaporizedFirst), destructedFirst(destructedFirst) {}

	_ArrayVaporizeGuard(const _ArrayVaporizeGuard&)				= delete;
	_ArrayVaporizeGuard& operator=(const _ArrayVaporizeGuard&)	= delete;

	constexpr ~_ArrayVaporizeGuard() noexcept {
		if (ptr) {
			memory::destruct(destructedFirst, ptr->last);
			ptr->last = vaporizedFirst;
		}
	}

	constexpr void release() noexcept {
		ptr = nullptr;
		vaporizedFirst	= nullptr;
		destructedFirst = nullptr;
	}

	DynamicArrVal* ptr;
	pointer vaporizedFirst;		// First element to be vaporized
	pointer destructedFirst;	// First element to be destructed
};

template<class T>
class DynamicArray {
public:
	using value_type		= T;
	using size_type			= std::size_t;
	using difference_type	= std::ptrdiff_t;
	using pointer			= T*;
	using const_pointer		= const T*;
	using reference			= T&;
	using const_reference	= const T&;

private:
	using _MyVal = _DynamicArrValue<value_type, size_type, difference_type, pointer, const_pointer>;

public:
	using iterator			= _DynamicArrayIterator<_MyVal>;
	using const_iterator	= _DynamicArrayConstIterator<_MyVal>;
	
	using reverse_iterator			= std::reverse_iterator<iterator>;
	using const_reverse_iterator	= std::reverse_iterator<const_iterator>;

public:
	constexpr DynamicArray() noexcept
		: _data() {}

	constexpr explicit DynamicArray(const size_type count)
		: _data() {
		this->_construct_n(count);
	}

	constexpr DynamicArray(const size_type count, const T& val)
		: _data() {
		this->_construct_n(count, val);
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	constexpr DynamicArray(It first, Se last)
		: _data() {
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::distance(first, last));
			this->_construct_n(count, std::move(first), std::move(last));
		}
		else {
			_ArrayConstructGuard<_MyVal> guard(std::addressof(_data));
			this->_append_uncounted_range(std::move(first), std::move(last));
			guard.release();
		}
	}

	constexpr DynamicArray(std::initializer_list<T> initList)
		: _data() {
		this->_construct_n(initList.size(), initList.begin(), initList.end());
	}

	constexpr DynamicArray(const DynamicArray& other)
		: _data() {
		this->_construct_n(other.size(), other.begin(), other.end());
	}

	constexpr DynamicArray(DynamicArray&& other) noexcept
		: _data() {
		_data.swap(other._data);
	}

	constexpr ~DynamicArray() noexcept {
		_data.clear();
	}

	constexpr DynamicArray& operator=(const DynamicArray& other) {
		if (this != std::addressof(other)) {
			this->_assign_counted_range(other._data.first, other.size());
		}
		return *this;
	}

	constexpr DynamicArray& operator=(DynamicArray&& other) {
		if (this != std::addressof(other)) {
			_data.clear();
			_data.swap(other._data);
		}
		return *this;
	}

	constexpr DynamicArray& operator=(std::initializer_list<T> initList) {
		this->_assign_counted_range(initList.begin(), initList.size());
		return *this;
	}

	[[nodiscard]] constexpr T& operator[](const size_type index) noexcept {
		return _data.first[index]; // UB: nullptr dereference
	}

	[[nodiscard]] constexpr const T& operator[](const size_type index) const noexcept {
		return _data.first[index];
	}

	[[nodiscard]] constexpr T& at(const size_type index) {
		if (index >= this->size()) {
			this->_subscription_error();
		}
		return _data.first[index];
	}

	[[nodiscard]] constexpr const T& at(const size_type index) const {
		if (index >= this->size()) {
			this->_subscription_error();
		}
		return _data.first[index];
	}

	[[nodiscard]] constexpr iterator begin() noexcept {
		return iterator(_data.first);
	}

	[[nodiscard]] constexpr const_iterator begin() const noexcept {
		return const_iterator(_data.first);
	}

	[[nodiscard]] constexpr iterator end() noexcept {
		return iterator(_data.last);
	}

	[[nodiscard]] constexpr const_iterator end() const noexcept {
		return const_iterator(_data.last);
	}

	[[nodiscard]] constexpr const_iterator cbegin() const noexcept {
		return this->begin();
	}

	[[nodiscard]] constexpr const_iterator cend() const noexcept {
		return this->end();
	}

	[[nodiscard]] constexpr reverse_iterator rbegin() noexcept {
		return reverse_iterator(this->end());
	}

	[[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept {
		return const_reverse_iterator(this->end());
	}

	[[nodiscard]] constexpr reverse_iterator rend() noexcept {
		return reverse_iterator(this->begin());
	}

	[[nodiscard]] constexpr const_reverse_iterator rend() const noexcept {
		return const_reverse_iterator(this->begin());
	}

	[[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept {
		return this->rbegin();
	}

	[[nodiscard]] constexpr const_reverse_iterator crend() const noexcept {
		return this->rend();
	}

	[[nodiscard]] constexpr T& front() {
		return *_data.first; // UB: nullptr dereference
	}

	[[nodiscard]] constexpr const T& front() const {
		return *_data.first;
	}

	[[nodiscard]] constexpr T& back() {
		return *(_data.last - 1); // UB: nullptr dereference
	}

	[[nodiscard]] constexpr const T& back() const {
		return *(_data.last - 1);
	}

	[[nodiscard]] constexpr T* data() noexcept {
		return _data.first;
	}

	[[nodiscard]] constexpr const T* data() const noexcept {
		return _data.first;
	}

	[[nodiscard]] constexpr bool is_empty() const noexcept {
		return _data.first == _data.last;
	}

	[[nodiscard]] constexpr size_type size() const noexcept {
		return static_cast<size_type>(_data.last - _data.first);
	}

	[[nodiscard]] constexpr size_type max_size() const noexcept {
		return std::min(
			static_cast<size_type>(std::numeric_limits<difference_type>::max()),	// Iterator arithmetic limit
			static_cast<size_type>(-1) / sizeof(T)									// Address space limit
		);
	}

	[[nodiscard]] constexpr size_type capacity() const noexcept {
		return static_cast<size_type>(_data.end - _data.first);
	}

	[[nodiscard]] constexpr size_type unused_capacity() const noexcept {
		return static_cast<size_type>(_data.end - _data.last);
	}

	template<class... Args>
	constexpr iterator emplace(const_iterator where, Args&&... args) {
		// Insert by perfectly forwarding args at where
		const pointer wherePtr	= where.ptr;
		const pointer oldLast	= _data.last;
		if (this->unused_capacity() > 0) {
			if (wherePtr == oldLast) { // At back, provide strong guarantee
				this->_emplace_back_with_unused_capacity(std::forward<Args>(args)...);
			}
			else {
				/*
				Create temporary element to handle aliasing
				E.g. arr.emplace(arr.begin() + 2, arr[4]);
												  -------
				*/
				memory::_TempObjectGuard<T> guard(std::forward<Args>(args)...);
				// Shift the last element to the right by 1 offset, potentially uninitialized memory
				memory::construct_at(oldLast, std::move(oldLast[-1]));
				++_data.last;
				// Shift range [wherePtr, oldLast - 1) to the right by 1 offset (shift backward to avoid overlap)
				memory::move_backward(wherePtr, oldLast - 1, oldLast);
				// Insert new element at where
				*wherePtr = std::move(guard.get_object());
			}
			return iterator(wherePtr);
		}
		return iterator(this->_emplace_reallocate(wherePtr, std::forward<Args>(args)...));
	}

	template<class... Args>
	constexpr reference emplace_back(Args&&... args) {
		// Insert by perfectly forwarding args at end, provide strong guarantee
		if (this->unused_capacity() > 0) {
			return this->_emplace_back_with_unused_capacity(std::forward<Args>(args)...);
		}
		return *this->_emplace_reallocate(_data.last, std::forward<Args>(args)...);
	}

	constexpr void push_back(const T& val) {
		// Insert by copying val at end, provide strong guarantee
		this->emplace_back(val);
	}

	constexpr void push_back(T&& val) {
		// Insert by moving val at end, provide strong guarantee
		this->emplace_back(std::move(val));
	}

	constexpr iterator insert(const_iterator where, const T& val) {
		// Insert by copying val at where
		return this->emplace(where, val);
	}

	constexpr iterator insert(const_iterator where, T&& val) {
		// Insert by moving val at where
		return this->emplace(where, std::move(val));
	}

	constexpr iterator insert(const_iterator where, const size_type count) {
		// Insert count * value-initialized at where
		return iterator(this->_insert(where.ptr, count));
	}

	constexpr iterator insert(const_iterator where, const size_type count, const T& val) {
		// Insert count * val at where
		return iterator(this->_insert(where.ptr, count, val));
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	constexpr iterator insert(const_iterator where, It first, Se last) {
		// Insert range [first, last) at where
		return iterator(this->_insert_range(where.ptr, std::move(first), std::move(last)));
	}

	constexpr iterator insert(const_iterator where, std::initializer_list<T> initList) {
		// Insert initList at where
		const auto offset = static_cast<size_type>(where.ptr - _data.first);
		this->_insert_counted_range(where.ptr, initList.begin(), initList.size());
		return iterator(_data.first + offset);
	}

	constexpr iterator append(const size_type count) {
		// Append count * value-initialized
		return iterator(this->_insert(_data.last, count));
	}

	constexpr iterator append(const size_type count, const T& val) {
		// Append count * val
		return iterator(this->_insert(_data.last, count, val));
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	constexpr iterator append(It first, Se last) {
		// Append range [first, last)
		return iterator(this->_insert_range(_data.last, std::move(first), std::move(last)));
	}

	constexpr iterator append(std::initializer_list<T> initList) {
		// Append initList
		return this->insert(this->end(), initList);
	}

	constexpr void assign(const size_type count) {
		// Assign count * value-initialized
		this->_assign(count, _ValueInitializeTag{});
	}

	constexpr void assign(const size_type count, const T& val) {
		// Assign count * val
		this->_assign(count, val);
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	constexpr void assign(It first, Se last) {
		// Assign range [first, last)
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::distance(first, std::move(last)));
			this->_assign_counted_range(std::move(first), count);
		}
		else {
			this->_assign_uncounted_range(std::move(first), std::move(last));
		}
	}

	constexpr void assign(const std::initializer_list<T> initList) {
		// Assign initList
		this->_assign_counted_range(initList.begin(), initList.size());
	}

	constexpr void pop_back() noexcept {
		// Erase the last element
		memory::destruct_at(--_data.last); // UB: nullptr dereference
	}

	constexpr iterator erase(const_iterator where)
		noexcept(std::is_nothrow_move_assignable_v<T>)
	{
		// Erase element at where
		pointer& myLast = _data.last;

		const pointer wherePtr = where.ptr;
		memory::move(wherePtr + 1, myLast, wherePtr);
		memory::destruct_at(--myLast);
		return iterator(wherePtr); // Make new iterator, where is already invalidated
	}

	constexpr iterator erase(const_iterator first, const_iterator last)
		noexcept(std::is_nothrow_move_assignable_v<T>)
	{
		// Erase range [first, last)
		pointer& myLast = _data.last;
		
		const pointer firstPtr	= first.ptr;
		const pointer lastPtr	= last.ptr;
		if (firstPtr != lastPtr) {
			const pointer newLast = memory::move(lastPtr, myLast, firstPtr).out;
			memory::destruct(newLast, myLast);
			myLast = newLast;
		}
		return iterator(firstPtr); // Make new iterator, first is already invalidated
	}

	constexpr void clear() noexcept {
		// Erase all elements and free all memory
		_data.clear();
	}

	constexpr void swap(DynamicArray& other) noexcept {
		// Swap with other
		if (this != std::addressof(other)) {
			_data.swap(other._data);
		}
	}

	constexpr void resize(const size_type newSize) {
		// Trim or append value-initialized elements to reach newSize, provide strong guarantee
		this->_resize(newSize, _ValueInitializeTag{});
	}

	constexpr void resize(const size_type newSize, const T& val) {
		// Trim or append copies of val to reach newSize, provide strong guarantee
		this->_resize(newSize, val);
	}

	constexpr void reserve(const size_type newCapacity) {
		// Expand capacity to newCapacity, provide strong guarantee
		if (newCapacity > this->capacity()) {
			if (newCapacity > this->max_size()) {
				this->_length_error();
			}

			this->_reallocate(newCapacity);
		}
	}

	constexpr void shrink_to_fit() {
		// Shrink capacity to size, provide strong guarantee
		if (this->unused_capacity() > 0) {
			if (this->is_empty()) {
				_data.clear();
			}
			else {
				this->_reallocate(this->size());
			}
		}
	}

private:
	constexpr void _allocate(const size_type newCapacity) {
		// Allocate array for newCapacity elements. Current array must be empty.
		if (newCapacity > this->max_size()) {
			this->_length_error();
		}

		const auto newFirst = static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
		
		_data.first = newFirst;
		_data.last	= newFirst;
		_data.end	= newFirst + newCapacity;
	}

	template<class... Args>
	constexpr void _construct_n(const size_type count, Args&&... args) {
		/*
		Dispatch between 3 construction methods:
			* 1-arg: value-construction			e.g. DynamicArray(5)
			* 2-arg: fill construction			e.g. DynamicArray(5, "meow")
			* 3-arg: sized range construction	e.g. DynamicArray({"Hello", "Fluffy", "World"})
		*/
		if (count == 0) {
			return;
		}

		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		this->_allocate(count);

		_ArrayConstructGuard<_MyVal> guard(std::addressof(_data));
		if constexpr (sizeof...(Args) == 0) {
			myLast = memory::uninitialized_default_construct_n(myFirst, count);
		}
		else if constexpr (sizeof...(Args) == 1) {
			myLast = memory::uninitialized_fill_n(myFirst, count, std::forward<Args>(args)...);
		}
		else if constexpr (sizeof...(Args) == 2) {
			myLast = memory::uninitialized_copy(std::forward<Args>(args)..., myFirst, myFirst + count).out;
		}
		else {
			static_assert(false, "Unexpected number of arguments");
		}
		guard.release();
	}

	constexpr void _change_array(pointer newFirst, const size_type newSize, const size_type newCapacity) noexcept {
		// Discard old array, acquire new array
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;
		pointer& myEnd		= _data.end;

		if (myFirst) {
			memory::destruct(myFirst, myLast);
			memory::deallocate(myFirst, this->capacity() * sizeof(T));
		}

		myFirst = newFirst;
		myLast	= newFirst + newSize;
		myEnd	= newFirst + newCapacity;
	}

	constexpr size_type _calculate_growth(const size_type newSize) const {
		// Given newSize, calculate geometric growth
		const auto maxSize		= this->max_size();
		const auto oldCapacity	= this->capacity();
		if (oldCapacity > maxSize - oldCapacity / 2) {
			return maxSize; // Geometric growth would overflow
		}

		const auto newCapacity = oldCapacity + oldCapacity / 2;
		if (newCapacity < newSize) {
			return newSize; // Geometric growth would be insufficient
		}
		return newCapacity; // Geometric growth is sufficient
	}

	template<class... Args>
	constexpr T& _emplace_back_with_unused_capacity(Args&&... args) {
		// Insert by perfectly forwarding into element at end
		pointer& myLast = _data.last;

		memory::construct_at(myLast, std::forward<Args>(args)...);
		T& result = *myLast++;
		return result;
	}

	template<class... Args>
	constexpr pointer _emplace_reallocate(pointer where, Args&&... args) {
		// Realllocate then insert by perfectly forwarding args at where
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto offset	= static_cast<size_type>(where - myFirst);
		const auto oldSize	= this->size();
		if (oldSize == this->max_size()) {
			this->_length_error();
		}

		const auto newSize			= oldSize + 1;
		const auto newCapacity		= this->_calculate_growth(newSize); // Calculate new capacity for array growth
		const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T))); // Allocate new array
		const auto constructedLast	= newFirst + offset + 1;
		// Set up guard
		_ArrayTransferGuard<_MyVal> guard(newCapacity, newFirst, constructedLast, constructedLast);
		memory::construct_at(newFirst + offset, std::forward<Args>(args)...);
		guard.constructedFirst = newFirst + offset;
		// Transfer elements into new array
		if (where == myLast) { // At back
			/*
			Move construction is faster than copy construction, but it only provides the basic guarantee.
			If an exception occurs partway through, the elements already moved into the new array cannot
			be rolled back(*), and the old array cannot be restored to its original state.

			(*): One might try to fix it by moving elements back into the old array, but that rollback
			uses the same move operations that just threw — so it may throw again. A guarantee is only
			"strong" if the rollback itself cannot fail.
			
			Copy construction never touches the old array during the process, so on exception we can simply
			discard the new array and roll back safely, giving the strong guarantee. The only downside is
			performance: copying can be slow when T is large or heavy.

			Unless T's move constructor is noexcept, or T cannot be copy constructed at all, we accept this
			tradeoff and favor the strong exception guarantee.
			*/
			if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) { // Basic guarantee
				memory::uninitialized_move(myFirst, myLast, newFirst, newFirst + oldSize);
			}
			else { // Strong guarantee when copy is possible
				memory::uninitialized_copy(myFirst, myLast, newFirst, newFirst + oldSize);
			}
		}
		else {
			memory::uninitialized_move(myFirst, where, newFirst, newFirst + oldSize);
			guard.constructedFirst = newFirst;

			const auto newOffset = newFirst + offset;
			memory::uninitialized_move(where, myLast, newOffset + 1, newOffset + 1 + (myLast - where));
		}
		guard.release(); // Guard has finished

		this->_change_array(newFirst, newSize, newCapacity);
		return newFirst + offset;
	}

	template<class... Args>
	constexpr pointer _insert(pointer where, const size_type count, const Args&... args) {
		// Insert count elements constructed from args at where
		if (count == 0) { // Do nothing, iterators won't be invalidated
			return where;
		}

		pointer& myLast = _data.last;

		const pointer oldFirst	= _data.first;
		const pointer oldLast	= _data.last;

		const auto offset		= static_cast<size_type>(where - oldFirst);
		const bool oneAtBack	= count == 1 && where == oldLast;
		if (count > this->unused_capacity()) { // Reallocate
			const auto oldSize = this->size();
			if (count > this->max_size() - oldSize) {
				this->_length_error();
			}

			const auto newSize			= oldSize + count;
			const auto newCapacity		= this->_calculate_growth(newSize);
			const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
			const auto constructedLast	= newFirst + offset + count;

			_ArrayTransferGuard<_MyVal> guard(newCapacity, newFirst, constructedLast, constructedLast);
			if constexpr (sizeof...(args) != 0) {
				memory::uninitialized_fill_n(newFirst + offset, count, args...);
			}
			else {
				memory::uninitialized_value_construct_n(newFirst + offset, count);
			}
			guard.constructedFirst = newFirst + offset;

			if (oneAtBack) {
				if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
					memory::uninitialized_move(oldFirst, oldLast, newFirst, newFirst + oldSize);
				}
				else {
					memory::uninitialized_copy(oldFirst, oldLast, newFirst, newFirst + oldSize);
				}
			}
			else {
				memory::uninitialized_move(oldFirst, where, newFirst, newFirst + offset);
				guard.constructedFirst = newFirst;
				memory::uninitialized_move(where, oldLast, newFirst + offset + count, newFirst + newSize);
			}
			guard.release();

			this->_change_array(newFirst, newSize, newCapacity);
		}
		else if (oneAtBack) {
			this->_emplace_back_with_unused_capacity(args...);
		}
		else {
			// Handle aliasing with temporary object guard
			const memory::_TempObjectGuard<T> guard(args...);
			const auto& object = guard.get_object();

			const auto affected = static_cast<size_type>(oldLast - where);
			if (count > affected) {
				// Fill (count - affected) * val into [oldLast, oldLast + count - affected), potentially uninitialized memory
				myLast = memory::uninitialized_fill_n(oldLast, count - affected, object);
				// Shift range [where, oldLast) to the right by count offset, potentially uninitialized memory
				myLast = memory::uninitialized_move(where, oldLast, where + count, oldLast + count).out;
				// Fill affected * val into [where, oldLast)
				memory::fill(where, oldLast, object);
			}
			else {
				// Shift range [oldLast - count, oldLast) to the right by count offset, potentially uninitialized memory
				myLast = memory::uninitialized_move(oldLast - count, oldLast, oldLast, oldLast + count).out;
				// Shift range [where, oldLast - count) backward to the right by count offset (shift backward to avoid overlap)
				memory::move_backward(where, oldLast - count, oldLast);
				// Fill count * val into [where, where + count)
				memory::fill_n(where, count, object);
			}
		}
		return _data.first + offset;
	}

	template<class It, class Se>
	constexpr void _append_uncounted_range(It first, const Se last) {
		// Insert uncounted range [first, last) at end
		for (; first != last; ++first) {
			this->emplace_back(*first); // If one at back, provide strong guarantee. Otherwise, provide basic guarantee
		}
	}

	template<class It, class Se>
	constexpr void _insert_uncounted_range(pointer where, It first, Se last) {
		// Insert unknown number of elements from [first, last) at where
		if (first == last) {
			return;
		}

		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto offset	= static_cast<size_type>(where - myFirst);
		const auto oldSize	= this->size();

		this->_append_uncounted_range(std::move(first), std::move(last));
		std::rotate(myFirst + offset, myFirst + oldSize, myLast);
	}

	template<class It>
	constexpr void _insert_counted_range(pointer where, It first, const size_type count) {
		// Insert elements from counted range [first, first + count) at where
		if (count == 0) { // Do nothing
			return;
		}

		pointer& myLast = _data.last;

		const pointer oldFirst	= _data.first;
		const pointer oldLast	= _data.last;

		const auto offset = static_cast<size_type>(where - oldFirst);
		if (count > this->unused_capacity()) { // Reallocate
			const auto oldSize = this->size();
			if (count > this->max_size() - oldSize) {
				this->_length_error();
			}

			const auto newSize			= oldSize + count;
			const auto newCapacity		= this->_calculate_growth(newSize);
			const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
			const auto constructedLast	= newFirst + offset + count;

			_ArrayTransferGuard<_MyVal> guard(newCapacity, newFirst, constructedLast, constructedLast);
			memory::uninitialized_copy_n(std::move(first), count, newFirst + offset, newFirst + offset + count);
			guard.constructedFirst = newFirst + offset;

			if (count == 1 && where == oldLast) { // One at back
				if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
					memory::uninitialized_move(oldFirst, oldLast, newFirst, newFirst + oldSize);
				}
				else {
					memory::uninitialized_copy(oldFirst, oldLast, newFirst, newFirst + oldSize);
				}
			}
			else {
				memory::uninitialized_move(oldFirst, where, newFirst, newFirst + offset);
				guard.constructedFirst = newFirst;
				memory::uninitialized_move(where, oldLast, newFirst + offset + count, newFirst + newSize);
			}
			guard.release();

			this->_change_array(newFirst, newSize, newCapacity);
		}
		else {
			/*
			The process here is similar to insert(where, count, val), but with a different requirement on T.
				- insert(where, first, last) only requires T to be EmplaceConstructible
				- insert(where, count, val) requires T to be CopyAssignable and CopyInsertable

			Thus, we need to turn range [where, where + count) into raw memory, then construct by copying from
			range [first, first + count), instead of assigning directly.
			*/
			const auto affected = static_cast<size_type>(oldLast - where);
			if (count >= affected) {
				// Shift the affected range to the right by count offset, potentially uninitialized memory
				myLast = memory::uninitialized_move(where, oldLast, where + count, where + count + affected).out;
				// Try to construct by copying [first, first + count) into [where, where + count), uninitialized memory
				memory::destruct(where, oldLast);
				try {
					memory::uninitialized_copy_n(std::move(first), count, where, where + count);
				}
				catch (...) {
					/*
					Copy construct failed, try to restore the array by shifting the chunks back into their original positions.

					VaporizedGuard is used to guard against double failure, which would leave the array in an invalid state.

					When this happens, all elements from [where, oldLast + count) will be vaporized. Due to double failure
					(fail to rollback a rollback), we can no longer provide strong guarantee. The least we can do is to make
					sure the array is in a valid state, by vaporizing all elements in the affected range.
					*/
					// Shift the affected range back into [where, oldLast), uninitialized memory
					_ArrayVaporizeGuard<_MyVal> guard(std::addressof(_data), where, where + count);
					memory::uninitialized_move(where + count, myLast, where, oldLast);
					guard.release();
					// Turn range [oldLast, oldLast + count) back into raw memory
					memory::destruct(oldLast, myLast);
					myLast = oldLast;
					throw;
				}
			}
			else {
				// Shift range [oldLast - count, oldLast) to the right by count offset, potentially uninitialized memory
				myLast = memory::uninitialized_move(oldLast - count, oldLast, oldLast, oldLast + count).out;
				// Shift range [where, oldLast - count) backward to the right by count offset
				memory::move_backward(where, oldLast - count, oldLast);
				
				memory::destruct(where, where + count);
				try {
					memory::uninitialized_copy_n(std::move(first), count, where, where + count);
				}
				catch (...) {
					// Shift the first count elements of the affected range back into [where, where + count)
					_ArrayVaporizeGuard<_MyVal> guard(std::addressof(_data), where, where + count);
					memory::uninitialized_move(where + count, where + 2 * count, where, where + count);
					guard.release();
					// Shift the remaining elements back into [where + count, oldLast)
					memory::move(where + 2 * count, myLast, where + count);

					memory::destruct(oldLast, myLast);
					myLast = oldLast;
					throw;
				}
			}
		}
	}

	template<class It, class Se>
	constexpr pointer _insert_range(pointer where, It first, Se last) {
		// Insert range [first, last) at where
		const auto offset = static_cast<size_type>(where.ptr - _data.first);
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::distance(first, std::move(last)));
			this->_insert_counted_range(where.ptr, std::move(first), count);
		}
		else {
			this->_insert_uncounted_range(where.ptr, std::move(first), std::move(last));
		}
		return _data.first + offset;
	}

	constexpr void _clear_reallocate(const size_type newSize) {
		// Clear and reallocate new array that grows to fit newSize
		if (newSize > this->max_size()) {
			this->_length_error();
		}

		const auto newCapacity = this->_calculate_growth(newSize); // Calculate growth before myEnd is reset
		
		this->clear();
		this->_allocate(newCapacity);
	}

	template<class OtherT>
	constexpr void _assign(const size_type count, const OtherT& arg) {
		// Assign count elements constructed from arg
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		if (count > this->capacity()) { // Reallocate
			this->_clear_reallocate(count);
			if constexpr (std::is_same_v<T, OtherT>) {
				myLast = memory::uninitialized_fill_n(myFirst, count, arg);
			}
			else {
				myLast = memory::uninitialized_value_construct_n(myFirst, count);
			}
			return;
		}

		const auto oldSize = this->size();
		if (count < oldSize) { // Fill and trim
			const pointer newLast = myFirst + count;
			if constexpr (std::is_same_v<T, OtherT>) {
				memory::fill(myFirst, newLast, arg);
			}
			else {
				memory::fill(myFirst, newLast, T{});
			}
			memory::destruct(newLast, myLast);
			myLast = newLast;
		}
		else { // Fill and append
			if constexpr (std::is_same_v<T, OtherT>) {
				memory::fill(myFirst, myLast, arg);
				myLast = memory::uninitialized_fill_n(myLast, count - oldSize, arg);
			}
			else {
				memory::fill(myFirst, myLast, T{});
				myLast = memory::uninitialized_value_construct_n(myLast, count - oldSize);
			}
		}
	}

	template<class It, class Se>
	constexpr void _assign_uncounted_range(It first, Se last) {
		// Assign unknown number of elements from [first, last)
		pointer& myLast = _data.last;
		// Reuse current elements
		pointer current = _data.first;
		for (; first != last && current != myLast; ++first, ++current) {
			*current = *first;
		}
		// Trim
		if (first == last) {
			memory::destruct(current, myLast);
			myLast = current;
			return;
		}
		// Append
		this->_append_uncounted_range(std::move(first), std::move(last));
	}

	template<class It>
	constexpr void _assign_counted_range(It first, const size_type newSize) {
		// Assign elements from counted range [first, first + newSize)
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;
		pointer& myEnd		= _data.end;

		if (newSize > this->capacity()) {
			this->_clear_reallocate(newSize);
			myLast = memory::uninitialized_copy_n(std::move(first), newSize, myFirst, myFirst + newSize).out;
			return;
		}

		const auto oldSize = this->size();
		if (newSize > oldSize) {
			bool isCopied = false;
			if constexpr (traits::iter_copy_category<It, pointer>::is_bitcopy_assignable) {
				if (!std::is_constant_evaluated()) {
					memory::_copy_memmove_n(first, myFirst, oldSize);
					first += oldSize;
					isCopied = true;
				}
			}

			if (!isCopied) {
				for (pointer current = myFirst; current != myLast; ++current, ++first) {
					*current = *first;
				}
			}

			myLast = memory::uninitialized_copy_n(
				std::move(first), newSize - oldSize, myLast, myLast + newSize - oldSize
			).out;
			return;
		}
		else {
			const pointer newLast = myFirst + newSize;
			memory::copy_n(std::move(first), newSize, myFirst);
			memory::destruct(newLast, myLast);
			myLast = newLast;
		}
	}

	template<class OtherT>
	constexpr void _resize_reallocate(const size_type newSize, const OtherT& val) {
		// Resize and reallocate new array that grows to fit newSize, provide strong guarantee
		if (newSize > this->max_size()) {
			this->_length_error();
		}

		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto oldSize			= this->size();
		const auto newCapacity		= this->_calculate_growth(newSize);
		const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
		const auto appendedFirst	= newFirst + oldSize;

		_ArrayTransferGuard<_MyVal> guard(newCapacity, newFirst, appendedFirst, appendedFirst);
		if constexpr (std::is_same_v<T, OtherT>) {
			guard.constructedLast = memory::uninitialized_fill_n(appendedFirst, newSize - oldSize, val);
		}
		else {
			guard.constructedLast = memory::uninitialized_value_construct_n(appendedFirst, newSize - oldSize);
		}

		if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
			memory::uninitialized_move(myFirst, myLast, newFirst, newFirst + oldSize);
		}
		else {
			memory::uninitialized_copy(myFirst, myLast, newFirst, newFirst + oldSize);
		}
		guard.release();

		this->_change_array(newFirst, newSize, newCapacity);
	}

	template<class OtherT>
	constexpr void _resize(const size_type newSize, const OtherT& arg) {
		// Trim or append elements constructed from arg to reach newSize, provide strong guarantee
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto oldSize = this->size();
		if (newSize < oldSize) { // Trim
			// Resize(0) erases all elements but keep allocated memory
			const pointer newLast = myFirst + newSize;
			memory::destruct(newLast, myLast);
			myLast = newLast;
			return;
		}

		if (newSize > oldSize) { // Append
			if (newSize > this->capacity()) { // Reallocate
				this->_resize_reallocate(newSize, arg);
				return;
			}

			const pointer oldLast = myLast;
			if constexpr (std::is_same_v<T, OtherT>) {
				myLast = memory::uninitialized_fill_n(oldLast, newSize - oldSize, arg);
			}
			else {
				myLast = memory::uninitialized_value_construct_n(oldLast, newSize - oldSize);
			}
		}

		// If newSize == oldSize, do nothing, iterators won't be invalidated
	}

	constexpr void _reallocate(const size_type newCapacity) {
		// Reallocate new array with newCapacity
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto oldSize	= this->size();
		const auto newFirst = static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));

		_ArrayReallocateGuard<_MyVal> guard(newCapacity, newFirst);
		if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
			memory::uninitialized_move(myFirst, myLast, newFirst, newFirst + oldSize);
		}
		else {
			memory::uninitialized_copy(myFirst, myLast, newFirst, newFirst + oldSize);
		}
		guard.release();

		this->_change_array(newFirst, oldSize, newCapacity);
	}

	[[noreturn]] static void _length_error() {
		throw std::length_error("Max size exceeded!");
	}

	[[noreturn]] static void _subscription_error() {
		throw std::out_of_range("Invalid subscription index!");
	}

private:
	_MyVal _data;
};

template<class T>
constexpr void swap(DynamicArray<T>& lhs, DynamicArray<T>& rhs) noexcept {
	lhs.swap(rhs);
}

template<class T>
[[nodiscard]] constexpr bool operator==(const DynamicArray<T>& lhs, const DynamicArray<T>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T>
[[nodiscard]] constexpr compare::SynthThreeWayCompareResult<T> operator<=>(
	const DynamicArray<T>& lhs, const DynamicArray<T>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // DYNAMIC_ARRAY_H