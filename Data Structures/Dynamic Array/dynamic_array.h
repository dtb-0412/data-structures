#pragma once
#ifndef DYNAMIC_ARRAY_H
#define DYNAMIC_ARRAY_H

#include"compare.hpp"
#include"memory.hpp"
#include"type_traits.hpp"

template<class DynamicArrVal>
class DynamicArrayConstIterator {
private:
	using _Pointer = typename DynamicArrVal::pointer;

public:
	using iterator_concept	= std::contiguous_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DynamicArrVal::value_type;
	using difference_type	= typename DynamicArrVal::difference_type;
	using pointer			= typename DynamicArrVal::pointer;
	using reference			= const value_type&;

	constexpr DynamicArrayConstIterator() noexcept
		: ptr() {}

	constexpr DynamicArrayConstIterator(_Pointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] constexpr reference operator*() const noexcept {
		return *ptr; // UB: nullptr or end() dereference
	}

	[[nodiscard]] constexpr pointer operator->() const noexcept {
		return ptr;
	}

	constexpr DynamicArrayConstIterator& operator++() noexcept {
		++ptr;  // UB: increment past end()
		return *this;
	}

	constexpr DynamicArrayConstIterator operator++(int) noexcept {
		DynamicArrayConstIterator temp = *this;
		++(*this);
		return temp;
	}

	constexpr DynamicArrayConstIterator& operator--() noexcept {
		--ptr;  // UB: decrement past begin()
		return *this;
	}

	constexpr DynamicArrayConstIterator operator--(int) noexcept {
		DynamicArrayConstIterator temp = *this;
		--(*this);
		return temp;
	}

	constexpr DynamicArrayConstIterator& operator+=(const difference_type offset) noexcept {
		ptr += offset;  // UB: increment past end()
		return *this;
	}

	[[nodiscard]] constexpr DynamicArrayConstIterator operator+(const difference_type offset) const noexcept {
		DynamicArrayConstIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] friend constexpr DynamicArrayConstIterator operator+(const difference_type offset, DynamicArrayConstIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	constexpr DynamicArrayConstIterator& operator-=(const difference_type offset) noexcept {
		ptr -= offset;
		return *this;
	}

	[[nodiscard]] constexpr DynamicArrayConstIterator operator-(const difference_type offset) const noexcept {
		DynamicArrayConstIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] constexpr difference_type operator-(const DynamicArrayConstIterator& other) const noexcept {
		return static_cast<difference_type>(ptr - other.ptr);  // UB: 2 iterators don't belong to the same container
	}

	[[nodiscard]] constexpr reference operator[](const difference_type offset) const noexcept {
		return *(*this + offset);  // UB: nullptr or end() dereference, offset out of range
	}

	[[nodiscard]] constexpr bool operator==(const DynamicArrayConstIterator& other) const noexcept {
		return ptr == other.ptr;  // UB: iterators don't belong to the same container
	}

	[[nodiscard]] constexpr std::strong_ordering operator<=>(const DynamicArrayConstIterator& other) const noexcept {
		return ptr <=> other.ptr;
	}

	_Pointer ptr;
};

template<class DynamicArrVal>
class DynamicArrayIterator : public DynamicArrayConstIterator<DynamicArrVal> {
private:
	using _BaseIter = DynamicArrayConstIterator<DynamicArrVal>;
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

	constexpr DynamicArrayIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	constexpr DynamicArrayIterator operator++(int) noexcept {
		DynamicArrayIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	constexpr DynamicArrayIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	constexpr DynamicArrayIterator operator--(int) noexcept {
		DynamicArrayIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}

	constexpr DynamicArrayIterator& operator+=(const difference_type offset) noexcept {
		_BaseIter::operator+=(offset);
		return *this;
	}

	[[nodiscard]] constexpr DynamicArrayIterator operator+(const difference_type offset) const noexcept {
		DynamicArrayIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] constexpr friend DynamicArrayIterator operator+(const difference_type offset, DynamicArrayIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	constexpr DynamicArrayIterator& operator-=(const difference_type offset) noexcept {
		_BaseIter::operator-=(offset);
		return *this;
	}

	// There are 2 overloads of _BaseIter::operator-(). If we override any one of them, the others will be hidden by default.
	// In this case, only the operator-(const difference_type) needs overriding.
	// Therefore we explicitly tell the compiler to inherit all other overloads except for the ones being overridden.
	using _BaseIter::operator-;

	[[nodiscard]] constexpr DynamicArrayIterator operator-(const difference_type offset) const noexcept {
		DynamicArrayIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] constexpr reference operator[](const difference_type offset) const noexcept {
		return const_cast<reference>(_BaseIter::operator[](offset));
	}
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr>
class DynamicArrValue {
public:
	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	constexpr DynamicArrValue() noexcept
		: first(), last(), end() {}

	constexpr DynamicArrValue(pointer first, pointer last, pointer end) noexcept
		: first(first), last(last), end(end) {}

	constexpr void swap(DynamicArrValue& other) noexcept {
		// Swap contents with other (ADL)
		using std::swap;
		swap(first, other.first);
		swap(last, other.last);
		swap(end, other.end);
	}

	constexpr void take_ownership(DynamicArrValue& other) noexcept {
		// Take ownership of other's contents, leaving other empty
		first	= std::exchange(other.first, nullptr);
		last	= std::exchange(other.last, nullptr);
		end		= std::exchange(other.end, nullptr);
	}

public:
	pointer first;	// Points to the beginning of the array
	pointer last;	// Points to the end of values in the array (size)
	pointer end;	// Points to the end of allocated memory in the array (capacity)
};

struct ValueInitializeTag {
	explicit ValueInitializeTag() = default;
};

template<class T>
struct [[nodiscard]] TempObjectGuard {
	template<class... Args>
	constexpr explicit TempObjectGuard(Args&&... args) {
		memory::construct_at(std::addressof(object), std::forward<Args>(args)...);
	}

	TempObjectGuard(const TempObjectGuard&)				= delete;
	TempObjectGuard& operator=(const TempObjectGuard&)	= delete;

	constexpr ~TempObjectGuard() noexcept {
		memory::destruct_at(std::addressof(object));
	}

	[[nodiscard]] constexpr T& getObject() noexcept {
		return object;
	}

	[[nodiscard]] constexpr const T& getObject() const noexcept {
		return object;
	}

	// Use union to manually manage object's construction and destruction, especially with allocator.
	union {
		T object;
	};
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
	using _DynamicArrValue = DynamicArrValue<value_type, size_type, difference_type, pointer, const_pointer>;

	struct [[nodiscard]] ConstructGuard {
		// Guard for construction failure
		constexpr ~ConstructGuard() noexcept {
			if (target) {
				target->_tidy();
			}
		}

		constexpr void release() noexcept {
			target = nullptr;
		}

		DynamicArray* target;
	};

	struct [[nodiscard]] ReallocateGuard {
		// Guard for reallocation failure
		constexpr ReallocateGuard(size_type newCapacity, pointer newFirst) noexcept
			: newCapacity(newCapacity), newFirst(newFirst) {
		}

		ReallocateGuard(const ReallocateGuard&)	= delete;
		ReallocateGuard(ReallocateGuard&&)		= delete;

		ReallocateGuard& operator=(const ReallocateGuard&)	= delete;
		ReallocateGuard& operator=(ReallocateGuard&&)		= delete;

		constexpr ~ReallocateGuard() noexcept {
			if (newFirst) {
				memory::deallocate(newFirst, newCapacity * sizeof(T));
			}
		}

		constexpr void release() noexcept {
			newFirst = nullptr;
		}

		size_type newCapacity;	// Capacity of the new array
		pointer newFirst;		// Points to the new array
	};

	struct [[nodiscard]] ArrayTransferGuard : public ReallocateGuard {
		// Guard for array transfer failure when inserting or resizing
		using _Base = ReallocateGuard;

		constexpr ArrayTransferGuard(size_type newCapacity, pointer newFirst, pointer constructedFirst, pointer constructedLast) noexcept
			: _Base(newCapacity, newFirst), constructedFirst(constructedFirst), constructedLast(constructedLast) {
		}

		constexpr ~ArrayTransferGuard() noexcept {
			if (constructedFirst) {
				memory::destruct(constructedFirst, constructedLast);
			}
		}

		constexpr void release() noexcept {
			constructedFirst	= nullptr;
			constructedLast		= nullptr;
			_Base::release();
		}

		pointer constructedFirst;	// Start of the constructed range
		pointer constructedLast;	// One-past-end of the constructed range
	};

	struct [[nodiscard]] VaporizeGuard {
		// Guard for double failure when inserting range of elements
		VaporizeGuard& operator=(const VaporizeGuard&)	= delete;
		VaporizeGuard& operator=(VaporizeGuard&&)		= delete;

		constexpr ~VaporizeGuard() noexcept {
			if (target) {
				pointer& myLast = target->_data.last;

				memory::destruct(destructedFirst, myLast);
				myLast = vaporizedFirst;
			}
		}

		constexpr void release() noexcept {
			target = nullptr;
		}

		DynamicArray* target;
		pointer vaporizedFirst;		// First element to be vaporized
		pointer destructedFirst;	// First element to be destructed
	};

public:
	using iterator			= DynamicArrayIterator<_DynamicArrValue>;
	using const_iterator	= DynamicArrayConstIterator<_DynamicArrValue>;
	
	using reverse_iterator			= std::reverse_iterator<iterator>;
	using const_reverse_iterator	= std::reverse_iterator<const_iterator>;

public:
	constexpr DynamicArray() noexcept
		: _data() {}

	constexpr explicit DynamicArray(size_type count)
		: _data() {
		this->_construct_n(count);
	}

	constexpr DynamicArray(size_type count, const T& val)
		: _data() {
		this->_construct_n(count, val);
	}

	template<class It,
		std::enable_if_t<std::input_or_output_iterator<It> && std::equality_comparable<It>, int> = 0>
	constexpr DynamicArray(It first, It last)
		: _data() {
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::distance(first, last));
			this->_construct_n(count, std::move(first), std::move(last));
		}
		else {
			ConstructGuard guard{ this };
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
		: _data(std::exchange(other._data.first, nullptr),
				std::exchange(other._data.last, nullptr),
				std::exchange(other._data.end, nullptr)) {}

	constexpr ~DynamicArray() noexcept {
		this->_tidy();
	}

	constexpr DynamicArray& operator=(const DynamicArray& other) {
		if (this != std::addressof(other)) {
			this->_assign_counted_range(other._data.first, other.size());
		}
		return *this;
	}

	constexpr DynamicArray& operator=(DynamicArray&& other) {
		if (this != std::addressof(other)) {
			this->_tidy();
			_data.take_ownership(other._data);
		}
		return *this;
	}

	constexpr DynamicArray& operator=(std::initializer_list<T> initList) {
		this->_assign_counted_range(initList.begin(), initList.size());
		return *this;
	}

	[[nodiscard]] constexpr T& operator[](const size_type index) {
		return _data.first[index]; // UB: nullptr dereference
	}

	[[nodiscard]] constexpr const T& operator[](const size_type index) const {
		return _data.first[index];
	}

	[[nodiscard]] constexpr T& at(const size_type index) {
		if (index >= size()) {
			this->_subscription_error();
		}
		return _data.first[index];
	}

	[[nodiscard]] constexpr const T& at(const size_type index) const {
		if (index >= size()) {
			this->_subscription_error();
		}
		return _data.first[index];
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

	[[nodiscard]] constexpr iterator begin() noexcept {
		return iterator(_data.first);
	}

	[[nodiscard]] constexpr const_iterator begin() const noexcept {
		return const_iterator(_data.first);
	}

	[[nodiscard]] constexpr const_iterator cbegin() const noexcept {
		return this->begin();
	}

	[[nodiscard]] constexpr iterator end() noexcept {
		return iterator(_data.last);
	}

	[[nodiscard]] constexpr const_iterator end() const noexcept {
		return const_iterator(_data.last);
	}

	[[nodiscard]] constexpr const_iterator cend() const noexcept {
		return this->end();
	}

	[[nodiscard]] constexpr reverse_iterator rbegin() noexcept {
		return reverse_iterator(end());
	}

	[[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept {
		return const_reverse_iterator(end());
	}

	[[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept {
		return this->rbegin();
	}

	[[nodiscard]] constexpr reverse_iterator rend() noexcept {
		return reverse_iterator(begin());
	}

	[[nodiscard]] constexpr const_reverse_iterator rend() const noexcept {
		return const_reverse_iterator(begin());
	}

	[[nodiscard]] constexpr const_reverse_iterator crend() const noexcept {
		return this->rend();
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

	template<class... Args>
	constexpr reference emplace_back(Args&&... args) {
		// Insert by perfectly forwarding args into element at end, provide strong guarantee
		return this->_emplace_back(std::forward<Args>(args)...);
	}

	constexpr void push_back(const T& val) {
		// Insert by copying val at end, provide strong guarantee
		this->_emplace_back(val);
	}

	constexpr void push_back(T&& val) {
		// Insert by moving val at end, provide strong guarantee
		this->_emplace_back(std::move(val));
	}

	template<class... Args>
	constexpr iterator emplace(const_iterator pos, Args&&... args) {
		// Insert by perfectly forwarding args at pos
		const pointer posPtr	= pos.ptr;
		const pointer oldLast	= _data.last;
		if (oldLast != _data.end) { // Has unused capacity
			if (posPtr == oldLast) { // At back, provide strong guarantee
				this->_emplace_back_with_unused_capacity(std::forward<Args>(args)...);
			}
			else {
				/*
				Create temporary element to handle aliasing
				E.g. arr.emplace(arr.begin() + 2, arr[4]);
												  -------
				*/
				TempObjectGuard<T> guard(std::forward<Args>(args)...);
				// Shift the last element to the right by 1 offset, potentially uninitialized memory
				memory::construct_at(oldLast, std::move(oldLast[-1]));
				++_data.last;
				// Shift range [posPtr, oldLast - 1) to the right by 1 offset (shift backward to avoid overlap)
				memory::move_backward(posPtr, oldLast - 1, oldLast);
				// Insert new element at pos
				*posPtr = std::move(guard.getObject());
			}
			return iterator(posPtr);
		}
		return iterator(this->_emplace_reallocate(posPtr, std::forward<Args>(args)...));
	}

	constexpr iterator insert(const_iterator pos, const T& val) {
		// Insert by copying val at pos
		return this->emplace(pos, val);
	}

	constexpr iterator insert(const_iterator pos, T&& val) {
		// Insert by moving val at pos
		return this->emplace(pos, std::move(val));
	}

	constexpr iterator insert(const_iterator pos, const size_type count, const T& val) {
		// Insert count * val at pos
		pointer& myLast = _data.last;

		const pointer posPtr	= pos.ptr;
		const pointer oldFirst	= _data.first;
		const pointer oldLast	= _data.last;

		const auto offset			= static_cast<size_type>(posPtr - oldFirst);
		const auto unusedCapacity	= static_cast<size_type>(_data.end - oldLast);
		const bool oneAtBack		= count == 1 && posPtr == oldLast;
		if (count > unusedCapacity) { // Reallocate
			const auto oldSize = this->size();
			if (count > this->max_size() - oldSize) {
				this->_length_error();
			}

			const auto newSize			= oldSize + count;
			const auto newCapacity		= this->_calculate_growth(newSize);
			const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
			const auto constructedLast	= newFirst + offset + count;

			ArrayTransferGuard guard{ newCapacity, newFirst, constructedLast, constructedLast };

			memory::uninitialized_fill_n(newFirst + offset, count, val);
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
				memory::uninitialized_move(oldFirst, posPtr, newFirst, newFirst + offset);
				guard.constructedFirst = newFirst;
				memory::uninitialized_move(posPtr, oldLast, newFirst + offset + count, newFirst + newSize);
			}
			guard.release();

			this->_change_array(newFirst, newSize, newCapacity);
		}
		else if (count == 0) {
			// Do nothing, iterators won't be invalidated
		}
		else if (oneAtBack) {
			this->_emplace_back_with_unused_capacity(val);
		}
		else {
			// Handle aliasing with temporary object guard
			const TempObjectGuard<T> guard{ val };
			const auto& object = guard.getObject();

			const auto affected = static_cast<size_type>(oldLast - posPtr);
			if (count > affected) {
				// Fill (count - affected) * val into [oldLast, oldLast + count - affected), potentially uninitialized memory
				myLast = memory::uninitialized_fill_n(oldLast, count - affected, object);
				// Shift range [posPtr, oldLast) to the right by count offset, potentially uninitialized memory
				const auto [_, out] = memory::uninitialized_move(posPtr, oldLast, posPtr + count, oldLast + count);
				myLast = out;
				// Fill affected * val into [posPtr, oldLast)
				memory::fill(posPtr, oldLast, object);
			}
			else {
				// Shift range [oldLast - count, oldLast) to the right by count offset, potentially uninitialized memory
				auto [_, out] = memory::uninitialized_move(oldLast - count, oldLast, oldLast, oldLast + count);
				myLast = out;
				// Shift range [posPtr, oldLast - count) backward to the right by count offset (shift backward to avoid overlap)
				memory::move_backward(posPtr, oldLast - count, oldLast);
				// Fill count * val into [posPtr, posPtr + count)
				memory::fill_n(posPtr, count, object);
			}
		}
		return iterator(_data.first + offset); // Initial posPtr is invalidated
	}

	template<class It,
		std::enable_if_t<std::input_or_output_iterator<It> && std::equality_comparable<It>, int> = 0>
	constexpr iterator insert(const_iterator pos, It first, It last) {
		// Insert range [first, last) at pos
		const auto offset = static_cast<size_type>(pos.ptr - _data.first);
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::distance(first, last));
			this->_insert_counted_range(pos, std::move(first), count);
		}
		else {
			this->_insert_uncounted_range(pos, std::move(first), std::move(last));
		}
		return iterator(_data.first + offset);
	}

	constexpr iterator insert(const_iterator pos, std::initializer_list<T> initList) {
		// Insert initList at pos
		const auto offset = static_cast<size_type>(pos.ptr - _data.first);
		this->_insert_counted_range(pos, initList.begin(), initList.size());
		return iterator(_data.first + offset);
	}

	constexpr void assign(const size_type newSize, const T& val) {
		// Assign newSize * val
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		if (newSize > this->capacity()) { // Reallocate
			this->_clear_reallocate(newSize);
			myLast = memory::uninitialized_fill_n(myFirst, newSize, val);
			return;
		}
		
		const auto oldSize = this->size();
		if (newSize > oldSize) { // Fill and append
			memory::fill(myFirst, myLast, val);
			myLast = memory::uninitialized_fill_n(myLast, newSize - oldSize, val);
		}
		else { // Fill and trim
			const pointer newLast = myFirst + newSize;
			memory::fill(myFirst, newLast, val);
			memory::destruct(newLast, myLast);
			myLast = newLast;
		}
	}

	template<class It,
		std::enable_if_t<std::input_or_output_iterator<It> && std::equality_comparable<It>, int> = 0>
	constexpr void assign(It first, It last) {
		// Assign range [first, last)
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::distance(first, last));
			this->_assign_counted_range(std::move(first), count);
			return;
		}
		else {
			this->_assign_uncounted_range(std::move(first), std::move(last));
		}
	}

	constexpr void assign(const std::initializer_list<T> initList) {
		// Assign range [initList.begin(), initList.end())
		this->_assign_counted_range(initList.begin(), initList.size());
	}

	constexpr void pop_back() noexcept {
		// Erase the last element
		memory::destruct_at(_data.last - 1); // UB: Array could be empty
		--_data.last;
	}

	constexpr iterator erase(const_iterator pos)
		noexcept(std::is_nothrow_move_assignable_v<T>)
	{
		// Erase element at pos
		pointer& myLast = _data.last;

		const pointer posPtr = pos.ptr;
		memory::move(posPtr + 1, myLast, posPtr);
		memory::destruct_at(myLast - 1);
		--myLast;
		return iterator(posPtr); // Make new iterator, pos is already invalidated
	}

	constexpr iterator erase(const_iterator first, const_iterator last)
		noexcept(std::is_nothrow_move_assignable_v<T>)
	{
		// Erase range [first, last)
		pointer& myLast = _data.last;
		
		const pointer firstPtr = first.ptr;
		const pointer lastPtr = last.ptr;
		if (firstPtr != lastPtr) {
			const auto [_, out] = memory::move(lastPtr, myLast, firstPtr);
			
			const pointer newLast = out;
			memory::destruct(newLast, myLast);
			myLast = newLast;
		}
		return iterator(firstPtr); // Make new iterator, first is already invalidated
	}

	constexpr void resize(const size_type newSize) {
		// Trim or append value-initialized elements to newSize, provide strong guarantee
		this->_resize(newSize, ValueInitializeTag{});
	}

	constexpr void resize(const size_type newSize, const T& val) {
		// Trim or append copies of val, provide strong guarantee
		this->_resize(newSize, val);
	}

	constexpr void reserve(const size_type newCapacity) {
		// Increase capacity to newCapacity, provide strong guarantee
		if (newCapacity > this->capacity()) {
			if (newCapacity > this->max_size()) {
				this->_length_error();
			}

			this->_reallocate(newCapacity);
		}
	}

	constexpr void shrink_to_fit() {
		// Shrink capacity to size, provide strong guarantee
		if (_data.last != _data.end) {
			if (this->is_empty()) {
				this->_tidy();
				return;
			}
			
			this->_reallocate(this->size());
		}
	}

	constexpr void clear() noexcept {
		// Erase all elements
		if (this->is_empty()) {
			return;
		}

		memory::destruct(_data.first, _data.last);
		_data.last = _data.first;
	}

	constexpr void swap(DynamicArray& other) noexcept {
		if (this != std::addressof(other)) {
			_data.swap(other._data);
		}
	}

private:
	constexpr void _allocate(size_type newCapacity) {
		// Allocate array for newCapacity elements
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
		Dispatch between 3 construction:
			* 1-arg: value-construction			e.g. DynamicArray(5)
			* 2-arg: fill construction			e.g. DynamicArray(5, "meow")
			* 3-arg: sized range construction	e.g. DynamicArray{"Hello", "Fluffy", "World"}
		*/
		if (count == 0) {
			return;
		}

		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		this->_allocate(count);

		ConstructGuard guard{ this };
		if constexpr (sizeof...(Args) == 0) {
			myLast = memory::uninitialized_default_construct_n(myFirst, count);
		}
		else if constexpr (sizeof...(Args) == 1) {
			myLast = memory::uninitialized_fill_n(myFirst, count, std::forward<Args>(args)...);
		}
		else if constexpr (sizeof...(Args) == 2) {
			const auto [_, out] = memory::uninitialized_copy(std::forward<Args>(args)..., myFirst, myFirst + count);
			myLast = out;
		}
		else {
			static_assert(false, "Unexpected number of arguments");
		}
		guard.release();
	}

	constexpr void _change_array(const pointer newFirst, const size_type newSize, const size_type newCapacity) noexcept {
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

	constexpr void _tidy() noexcept {
		// Clear all elements and deallocate
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;
		pointer& myEnd		= _data.end;

		if (myFirst) {
			memory::destruct(myFirst, myLast);
			memory::deallocate(myFirst, this->capacity() * sizeof(T));
			
			myFirst = nullptr;
			myLast	= nullptr;
			myEnd	= nullptr;
		}
	}

	template<class... Args>
	constexpr T& _emplace_back_with_unused_capacity(Args&&... args) {
		// Insert by perfectly forwarding into element at end
		pointer& myLast = _data.last;

		memory::construct_at(myLast, std::forward<Args>(args)...);
		
		T& result = *myLast;
		++myLast;
		return result;
	}

	template<class... Args>
	constexpr pointer _emplace_reallocate(const pointer pos, Args&&... args) {
		// Realllocate then insert by perfectly forwarding args at pos
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto offset	= static_cast<size_type>(pos - myFirst);
		const auto oldSize	= this->size();
		if (oldSize == this->max_size()) {
			this->_length_error();
		}

		const auto newSize			= oldSize + 1;
		const auto newCapacity		= this->_calculate_growth(newSize);
		const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
		const auto constructedLast	= newFirst + offset + 1;

		ArrayTransferGuard guard{ newCapacity, newFirst, constructedLast, constructedLast };

		memory::construct_at(newFirst + offset, std::forward<Args>(args)...);
		guard.constructedFirst = newFirst + offset;

		if (pos == myLast) {
			if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
				memory::uninitialized_move(myFirst, myLast, newFirst, newFirst + oldSize);
			}
			else {
				memory::uninitialized_copy(myFirst, myLast, newFirst, newFirst + oldSize);
			}
		}
		else {
			memory::uninitialized_move(myFirst, pos, newFirst, newFirst + oldSize);
			guard.constructedFirst = newFirst;

			const auto newPos = newFirst + offset;
			memory::uninitialized_move(pos, myLast, newPos + 1, newPos + 1 + (myLast - pos));
		}
		guard.release();

		this->_change_array(newFirst, newSize, newCapacity);
		return newFirst + offset;
	}

	template<class... Args>
	constexpr T& _emplace_back(Args&&... args) {
		// Insert by perfectly forwarding into element at end
		if (_data.last != _data.end) {
			return this->_emplace_back_with_unused_capacity(std::forward<Args>(args)...);
		}
		return *this->_emplace_reallocate(_data.last, std::forward<Args>(args)...);
	}

	template<class It, class Se>
	constexpr void _append_uncounted_range(It first, const Se last) {
		// Insert uncounted range [first, last) at end
		for (; first != last; ++first) {
			this->_emplace_back(*first); // If one at back, provide strong guarantee. Otherwise, provide basic guarantee
		}
	}

	constexpr size_type _calculate_growth(const size_type newSize) const {
		// Given newSize, calculate geometric growth
		const auto oldCapacity	= this->capacity();
		const auto maxSize		= this->max_size();

		if (oldCapacity > maxSize - oldCapacity / 2) {
			return maxSize; // Geometric growth would overflow
		}

		const auto newCapacity = oldCapacity + oldCapacity / 2;
		if (newCapacity < newSize) {
			return newSize; // Geometric growth would be insufficient
		}
		return newCapacity; // Geometric growth is sufficient
	}

	constexpr void _reallocate(const size_type newCapacity) {
		// Reallocate new array with newCapacity
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto size		= this->size();
		const auto newFirst = static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));

		ReallocateGuard guard{ newCapacity, newFirst };
		if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
			memory::uninitialized_move(myFirst, myLast, newFirst, newFirst + size);
		}
		else {
			memory::uninitialized_copy(myFirst, myLast, newFirst, newFirst + size);
		}
		guard.release();

		this->_change_array(newFirst, this->size(), newCapacity);
	}

	constexpr void _clear_reallocate(const size_type newSize) {
		// Clear and reallocate new array that grows to fit newSize
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;
		pointer& myEnd		= _data.end;

		if (newSize > this->max_size()) {
			this->_length_error();
		}

		const auto newCapacity = this->_calculate_growth(newSize); // Calculate growth before myEnd is reset

		if (myFirst) { // Destruct and deallocate old array
			memory::destruct(myFirst, myLast);
			memory::deallocate(myFirst, this->capacity() * sizeof(T));

			myFirst	= nullptr;
			myLast	= nullptr;
			myEnd	= nullptr;
		}
		this->_allocate(newCapacity);
	}

	template<class It, class Se>
	constexpr void _insert_uncounted_range(const_iterator pos, It first, Se last) {
		// Insert unknown number of elements from [first, last) at pos
		if (first == last) {
			return;
		}

		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto offset	= static_cast<size_type>(pos.ptr - myFirst);
		const auto size		= this->size();

		this->_append_uncounted_range(std::move(first), std::move(last));
		std::rotate(myFirst + offset, myFirst + size, myLast);
	}

	template<class It>
	constexpr void _insert_counted_range(const_iterator pos, It first, const size_type count) {
		// Insert elements from counted range [first, first + count) at pos
		pointer& myLast = _data.last;
		
		const pointer posPtr	= pos.ptr;
		const pointer oldFirst	= _data.first;
		const pointer oldLast	= _data.last;

		const auto offset			= static_cast<size_type>(posPtr - oldFirst);
		const auto unusedCapacity	= static_cast<size_type>(_data.end - oldLast);
		const bool oneAtBack		= count == 1 && posPtr == oldLast;
		if (count > unusedCapacity) { // Reallocate
			const auto oldSize = this->size();
			if (count > this->max_size() - oldSize) {
				this->_length_error();
			}

			const auto newSize			= oldSize + count;
			const auto newCapacity		= this->_calculate_growth(newSize);
			const auto newFirst			= static_cast<pointer>(memory::allocate(newCapacity, sizeof(T)));
			const auto constructedLast	= newFirst + offset + count;

			ArrayTransferGuard guard{ newCapacity, newFirst, constructedLast, constructedLast };

			memory::uninitialized_copy_n(std::move(first), count, newFirst + offset, newFirst + offset + count);
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
				memory::uninitialized_move(oldFirst, posPtr, newFirst, newFirst + offset);
				guard.constructedFirst = newFirst;
				memory::uninitialized_move(posPtr, oldLast, newFirst + offset + count, newFirst + newSize);
			}
			guard.release();

			this->_change_array(newFirst, newSize, newCapacity);
		}
		else if (count == 0) {
			// Do nothing
		}
		else {
			/*
			The process here is similar to insert(pos, count, val), but with a different requirement on T.
				- insert(pos, first, last) only requires T to be EmplaceConstructible
				- insert(pos, count, val) requires T to be CopyAssignable and CopyInsertable

			Thus, we need to turn range [pos, pos + count) into raw memory, then construct by copying from
			[first, first + count), instead of assigning directly.
			*/
			const auto affected = static_cast<size_type>(oldLast - posPtr);
			if (count >= affected) {
				// Shift the affected range to the right by count offset, potentially uninitialized memory
				const auto [_, out] = memory::uninitialized_move(posPtr, oldLast, posPtr + count, posPtr + count + affected);
				myLast = out;
				// Try to construct by copying [first, first + count) into [posPtr, posPtr + count), uninitialized memory
				memory::destruct(posPtr, oldLast);
				try {
					memory::uninitialized_copy_n(std::move(first), count, posPtr, posPtr + count);
				}
				catch (...) {
					/*
					Copy construct failed, try to restore the array by shifting the chunks back into their original positions.

					VaporizedGuard is used to guard against double failure, which would leave the array in an invalid state.

					When this happens, all elements from [posPtr, oldLast + count) will be vaporized. Due to double failure
					(fail to rollback a rollback), we can no longer provide strong guarantee. The least we can do is to make
					sure the array is in a valid state, by vaporizing all elements in the affected range.
					*/

					// Shift the affected range back into [posPtr, oldLast), uninitialized memory
					VaporizeGuard guard{ this, posPtr, posPtr + count };
					memory::uninitialized_move(posPtr + count, myLast, posPtr, oldLast);
					guard.release();
					// Turn range [oldLast, oldLast + count) back into raw memory
					memory::destruct(oldLast, myLast);
					myLast = oldLast;
					throw;
				}
			}
			else {
				// Shift range [oldLast - count, oldLast) to the right by count offset, potentially uninitialized memory
				const auto [_, out] = memory::uninitialized_move(oldLast - count, oldLast, oldLast, oldLast + count);
				myLast = out;
				// Shift range [posPtr, oldLast - count) backward to the right by count offset
				memory::move_backward(posPtr, oldLast - count, oldLast);
				
				memory::destruct(posPtr, posPtr + count);
				try {
					memory::uninitialized_copy_n(std::move(first), count, posPtr, posPtr + count);
				}
				catch (...) {
					// Shift the first count elements of the affected range back into [posPtr, posPtr + count)
					VaporizeGuard guard{ this, posPtr, posPtr + count };
					memory::uninitialized_move(posPtr + count, posPtr + 2 * count, posPtr, posPtr + count);
					guard.release();
					// Shift the remaining elements back into [posPtr + count, oldLast)
					memory::move(posPtr + 2 * count, myLast, posPtr + count);

					memory::destruct(oldLast, myLast);
					myLast = oldLast;
					throw;
				}
			}
		}
	}

	template<class It, class Se>
	constexpr void _assign_uncounted_range(It first, Se last) {
		// Assign unknown number of elements from [first, last)
		pointer& myLast = _data.last;

		pointer current = _data.first;
		for (; first != last && current != myLast; ++first, ++current) {
			*current = *first;
		}

		/*
		- If exhausted only the source: Trim, then Append does nothing
		- If exhausted only the dest: Append, then Trim does nothing
		- If exhausted both ranges: Trim does nothing, then Append does nothing
		*/
		
		// Trim
		memory::destruct(current, myLast);
		myLast = current;

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
			const auto [_, out] = memory::uninitialized_copy_n(std::move(first), newSize, myFirst, myFirst + newSize);
			myLast = out;
			return;
		}

		const auto oldSize = this->size();
		if (newSize > oldSize) {
			bool isCopied = false;
			if constexpr (traits::iter_copy_category<It, pointer>::is_bitcopy_assignable) {
				if (!std::is_constant_evaluated()) {
					memory::_copy_memmove_n(first, myFirst, static_cast<std::size_t>(oldSize));
					first += oldSize;
					isCopied = true;
				}
			}

			if (!isCopied) {
				for (auto current = myFirst; current != myLast; ++current, ++first) {
					*current = *first;
				}
			}

			const auto remaining = newSize - oldSize;
			const auto [_, out] = memory::uninitialized_copy_n(std::move(first), remaining, myLast, myLast + remaining);
			myLast = out;
			return;
		}
		else {
			const pointer newLast = myFirst + newSize;
			memory::copy_n(std::move(first), newSize, myFirst);
			memory::destruct(newLast, myLast);
			myLast = newLast;
		}
	}

	template<class T2>
	constexpr void _resize_reallocate(const size_type newSize, const T2& val) {
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

		ArrayTransferGuard guard{ newCapacity, newFirst, appendedFirst, appendedFirst };

		if constexpr (std::is_same_v<T, T2>) {
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

	template<class T2>
	constexpr void _resize(const size_type newSize, const T2& val) {
		// Trim or append elements, provide strong guarantee
		pointer& myFirst	= _data.first;
		pointer& myLast		= _data.last;

		const auto oldSize = this->size();
		if (newSize < oldSize) { // Trim
			const pointer newLast = myFirst + newSize;
			memory::destruct(newLast, myLast);
			myLast = newLast;
			return;
		}

		if (newSize > oldSize) { // Append
			if (newSize > this->capacity()) { // Reallocate
				this->_resize_reallocate(newSize, val);
				return;
			}

			const pointer oldLast = myLast;
			if constexpr (std::is_same_v<T, T2>) {
				myLast = memory::uninitialized_fill_n(oldLast, newSize - oldSize, val);
			}
			else {
				myLast = memory::uninitialized_value_construct_n(oldLast, newSize - oldSize);
			}
		}

		// If newSize == oldSize, do nothing, iterators won't be invalidated
	}

	[[noreturn]] static void _length_error() {
		throw std::length_error("Max size exceeded!");
	}

	[[noreturn]] static void _subscription_error() {
		throw std::out_of_range("Invalid subscription index!");
	}

private:
	_DynamicArrValue _data;
};

template<class T>
[[nodiscard]] constexpr bool operator==(const DynamicArray<T>& lhs, const DynamicArray<T>& rhs) {
	if (lhs.size() != rhs.size()) {
		return false;
	}
	return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T>
[[nodiscard]] constexpr compare::SynthThreeWayCompareResult<T> operator<=>(
	const DynamicArray<T>& lhs, const DynamicArray<T>& rhs
) {
	return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{});
}

template<class T>
constexpr void swap(DynamicArray<T>& lhs, DynamicArray<T>& rhs) noexcept {
	lhs.swap(rhs);
}
#endif // DYNAMIC_ARRAY_H