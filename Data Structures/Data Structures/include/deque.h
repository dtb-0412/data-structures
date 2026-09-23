#pragma once
#ifndef DEQUE_H
#define DEQUE_H

#include"compare.hpp"
#include"memory.hpp"

#include<bit>
#include<iostream>
#include<iomanip>

template<class DequeVal>
class _DequeConstIterator {
private:
	using _SizeType = typename DequeVal::size_type;

	//static constexpr int _blockSize = DequeVal::_blockSize;

public:
	using iterator_concept	= std::random_access_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DequeVal::value_type;
	using difference_type	= typename DequeVal::difference_type;
	using pointer			= typename DequeVal::const_pointer;
	using reference			= const value_type&;

	_DequeConstIterator() noexcept
		: offset(0), data()  {}

	_DequeConstIterator(const _SizeType offset, const DequeVal& data) noexcept
		: offset(offset), data(std::addressof(data)) {
	}

	[[nodiscard]] reference operator*() const noexcept {
		return data->subscript(offset);
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_DequeConstIterator& operator++() noexcept {
		++offset;
		return *this;
	}

	_DequeConstIterator operator++(int) noexcept {
		_DequeConstIterator temp = *this;
		++(*this);
		return temp;
	}

	_DequeConstIterator& operator--() noexcept {
		--offset;
		return *this;
	}

	_DequeConstIterator operator--(int) noexcept {
		_DequeConstIterator temp = *this;
		--(*this);
		return temp;
	}

	_DequeConstIterator& operator+=(const difference_type offset) noexcept {
		this->offset = static_cast<_SizeType>(this->offset + offset);
		return *this;
	}

	[[nodiscard]] _DequeConstIterator operator+(const difference_type offset) const noexcept {
		_DequeConstIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] friend _DequeConstIterator operator+(const difference_type offset, _DequeConstIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	_DequeConstIterator& operator-=(const difference_type offset) noexcept {
		this->offset = static_cast<_SizeType>(this->offset - offset);
		return *this;
	}

	[[nodiscard]] _DequeConstIterator operator-(const difference_type offset) const noexcept {
		_DequeConstIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] difference_type operator-(const _DequeConstIterator& other) const noexcept {
		return static_cast<difference_type>(offset - other.offset);
	}

	[[nodiscard]] reference operator[](const difference_type offset) const noexcept {
		return *(*this + offset);
	}

	[[nodiscard]] bool operator==(const _DequeConstIterator& other) const noexcept {
		return offset == other.offset;
	}

	[[nodiscard]] std::strong_ordering operator<=>(const _DequeConstIterator& other) const noexcept {
		return offset <=> other.offset;
	}

public:
	_SizeType offset;

	const DequeVal* data;
};

template<class DequeVal>
class _DequeIterator : public _DequeConstIterator<DequeVal> {
private:
	using _SizeType = typename DequeVal::size_type;

	using _BaseIter = _DequeConstIterator<DequeVal>;
	using _BaseIter::_BaseIter;  // Inherit _BaseIter's constructors

public:
	using iterator_concept	= std::random_access_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DequeVal::value_type;
	using difference_type	= typename DequeVal::difference_type;
	using pointer			= typename DequeVal::pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_DequeIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	_DequeIterator operator++(int) noexcept {
		_DequeIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	_DequeIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	_DequeIterator operator--(int) noexcept {
		_DequeIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}

	_DequeIterator& operator+=(const difference_type offset) noexcept {
		_BaseIter::operator+=(offset);
		return *this;
	}

	[[nodiscard]] _DequeIterator operator+(const difference_type offset) const noexcept {
		_DequeIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] friend _DequeIterator operator+(const difference_type offset, _DequeIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	_DequeIterator& operator-=(const difference_type offset) noexcept {
		_BaseIter::operator-=(offset);
		return *this;
	}

	using _BaseIter::operator-;

	[[nodiscard]] _DequeIterator operator-(const difference_type offset) const noexcept {
		_DequeIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] reference operator[](const difference_type offset) const noexcept {
		return const_cast<reference>(_BaseIter::operator[](offset));
	}
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class MapPtr>
struct _DequeValue {
public:
	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;
	using reference			= value_type&;
	using const_reference	= const value_type&;

private:
	using _MapPointer	= MapPtr;
	using _MapDiffType	= typename std::iterator_traits<_MapPointer>::difference_type;

	static constexpr std::size_t _bytes	= sizeof(value_type);
	
	// Each block can hold up to 1024 elements
	static constexpr std::size_t _maxBlockSize = 1024;
	
	/*
	Raw number of elements per block.
	Normally, compiler automatically adds padding (Data Structure Alignment) bytes to make total object's size
	multiple of 4 or 8 bytes, so that CPU can access memory at optimal speed.
	Therefore, we don't usually have non-"power of 2" object size.

	In case we do have: we calculate the raw number of elements per block (the non-"power of 2" block size),
	then use std::bit_floor() to find the largest power of 2 <= the calculated raw block size.
	Example:
		_bytes			= sizeof(_13BytesStruct) 
						= 13;
		_rawBlockSize	= 1024 / 13
						= 78;
		_blockSize		= std::bit_floor(78)
						= 2^6
						= 64 (<= 78);
	*/
	static constexpr std::size_t _rawBlockSize = (_bytes < _maxBlockSize) ? _maxBlockSize / _bytes : 1;

public:
	// Number of elements per block, power of 2, scale with element size
	
	// In MSVC, std::deque has a maximum block size of 16 elements.
	static constexpr std::size_t _blockSize = _bytes <= 1 ? 16
											: _bytes <= 2 ? 8
											: _bytes <= 4 ? 4
											: _bytes <= 8 ? 2
											:				1;
	
	// static constexpr std::size_t _blockSize = std::bit_floor(_rawBlockSize);

	_DequeValue() noexcept
		: map(), mapSize(0), index(0), size(0) {}

	_MapDiffType get_block_offset(const size_type index) const noexcept {
		// Get block offset in map from element index
		return static_cast<_MapDiffType>((index / _blockSize) & (mapSize - 1));
	}

	difference_type get_elem_offset(const size_type index) const noexcept {
		// Get element offset in block from element index
		return static_cast<difference_type>(index % _blockSize);
	}

	value_type* get_address(const size_type index) noexcept {
		// Get address of element at index
		const auto blockOffset	= this->get_block_offset(index);
		const auto elemOffset	= this->get_elem_offset(index);
		return map[blockOffset] + elemOffset;
	}

	reference subscript(const size_type index) noexcept {
		// Get element at index
		const auto blockOffset	= this->get_block_offset(index);
		const auto elemOffset	= this->get_elem_offset(index);
		return map[blockOffset][elemOffset];
	}

	const_reference subscript(const size_type index) const noexcept {
		// Get element at index
		const auto blockOffset	= this->get_block_offset(index);
		const auto elemOffset	= this->get_elem_offset(index);
		return map[blockOffset][elemOffset];
	}

	void swap(_DequeValue& other) noexcept {
		using std::swap;
		swap(map, other.map);
		swap(mapSize, other.mapSize);
		swap(index, other.index);
		swap(size, other.size);
	}

	_MapPointer map;	// Pointer to array of pointers to blocks
	
	size_type mapSize;	// Size of map array (number of blocks)
	size_type index;	// Index of initial element in deque
	size_type size;		// Number of elements in deque

	/*
	Naming convention:
		Array of block pointers:			map
		Number of block pointers in map:	mapSize/myMapSize/newMapSize
		Number of elements in deque:		size/mySize/newSize
		Number of elements in block:		blockSize, _blockSize
		Index of first block/elem in map:	index/myIndex/newIndex
		Index of block in map:				elemOffset
		Index of element in block:			elemOffset
		Index of element in deque:			index/newIndex
	*/
};

template<class Deque>
struct _DequeConstructGuard {
	// Guard for deque construction failure
	_DequeConstructGuard(Deque* target)
		: target(target) {}

	_DequeConstructGuard(const _DequeConstructGuard&)				= delete;
	_DequeConstructGuard& operator=(const _DequeConstructGuard&)	= delete;

	constexpr ~_DequeConstructGuard() noexcept {
		if (target) {
			target->clear();
		}
	}

	constexpr void release() noexcept {
		target = nullptr;
	}

	Deque* target;
};

template<class T>
class Deque {
public:
	using value_type		= T;
	using size_type			= std::size_t;
	using difference_type	= std::ptrdiff_t;
	using pointer			= T*;
	using const_pointer		= const T*;
	using reference			= T&;
	using const_reference	= const T&;

private:
	using _BlockPointer = T*;
	using _MapPointer	= T**;
	using _MapDiffType	= typename std::iterator_traits<_MapPointer>::difference_type;

	using _MyVal = _DequeValue<value_type, size_type, difference_type, pointer, const_pointer, _MapPointer>;

	static constexpr int _minMapSize	= 8;
	static constexpr int _blockSize		= _MyVal::_blockSize;

public:
	using iterator			= _DequeIterator<_MyVal>;
	using const_iterator	= _DequeConstIterator<_MyVal>;

	using reverse_iterator			= std::reverse_iterator<iterator>;
	using const_reverse_iterator	= std::reverse_iterator<const_iterator>;

public:
	Deque()
		: _data() {}

	explicit Deque(const size_type count)
		: _data() {
		this->_construct_n(count);
	}

	Deque(const size_type count, const T& val)
		: _data() {
		this->_construct_n(count, val);
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	Deque(It first, Se last)
		: _data() {
		this->_construct_range(std::move(first), std::move(last));
	}

	Deque(std::initializer_list<T> initList)
		: _data() {
		this->_construct_range(initList.begin(), initList.end());
	}

	Deque(const Deque& other)
		: _data() {
		this->_construct_range(other.begin(), other.end());
	}

	Deque(Deque&& other) noexcept
		: _data() {
		_data.swap(other._data);
	}

	~Deque() noexcept {
		this->clear();
	}

	Deque& operator=(const Deque& other) {
		if (this != std::addressof(other)) {
			this->_assign(other.begin(), other.end());
		}
		return *this;
	}

	Deque& operator=(Deque&& other) noexcept {
		if (this != std::addressof(other)) {
			this->clear();
			_data.swap(other._data);
		}
		return *this;
	}

	Deque& operator=(std::initializer_list<T> initList) {
		this->_assign(initList.begin(), initList.end());
		return *this;
	}

	[[nodiscard]] T& operator[](const size_type index) noexcept {
		return _data.subscript(index); // UB: nullptr dereference
	}

	[[nodiscard]] const T& operator[](const size_type index) const noexcept {
		return _data.subscript(index);
	}

	[[nodiscard]] T& at(const size_type index) {
		if (index >= _data.size) {
			this->_subscription_error();
		}
		return _data.subscript(index);
	}

	[[nodiscard]] const T& at(const size_type index) const {
		if (index >= _data.size) {
			this->_subscription_error();
		}
		return _data.subscript(index);
	}

	[[nodiscard]] iterator begin() noexcept {
		return iterator(_data.index, _data);
	}

	[[nodiscard]] const_iterator begin() const noexcept {
		return const_iterator(_data.index, _data);
	}

	[[nodiscard]] iterator end() noexcept {
		return iterator(_data.index + _data.size, _data);
	}

	[[nodiscard]] const_iterator end() const noexcept {
		return const_iterator(_data.index + _data.size, _data);
	}

	[[nodiscard]] const_iterator cbegin() const noexcept {
		return const_iterator(this->begin());
	}

	[[nodiscard]] const_iterator cend() const noexcept {
		return const_iterator(this->end());
	}

	[[nodiscard]] reverse_iterator rbegin() noexcept {
		return reverse_iterator(this->end());
	}

	[[nodiscard]] const_reverse_iterator rbegin() const noexcept {
		return const_reverse_iterator(this->end());
	}

	[[nodiscard]] reverse_iterator rend() noexcept {
		return reverse_iterator(this->begin());
	}

	[[nodiscard]] const_reverse_iterator rend() const noexcept {
		return const_reverse_iterator(this->begin());
	}

	[[nodiscard]] const_reverse_iterator crbegin() const noexcept {
		return this->rbegin();
	}

	[[nodiscard]] const_reverse_iterator crend() const noexcept {
		return this->rend();
	}

	[[nodiscard]] T& front() noexcept {
		return _data.subscript(0); // UB: nullptr dereference
	}

	[[nodiscard]] const T& front() const noexcept {
		return _data.subscript(0);
	}

	[[nodiscard]] T& back() noexcept {
		return _data.subscript(_data.size - 1); // UB: nullptr dereference
	}

	[[nodiscard]] const T& back() const noexcept {
		return _data.subscript(_data.size - 1);
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.size == 0;
	}

	[[nodiscard]] size_type size() const noexcept {
		return _data.size;
	}

	[[nodiscard]] constexpr size_type max_size() const noexcept {
		return std::min(
			static_cast<size_type>(std::numeric_limits<difference_type>::max()),
			static_cast<size_type>(-1) / sizeof(T)
		);
	}

	void assign(size_type count, const T& val) {
		// Assign count * val
		const auto end = this->end();
		for (auto begin = this->begin(); begin != end; ++begin, --count) {
			if (count == 0) { // Trim excessive elements
				auto remaining = static_cast<size_type>(end - begin);
				for (; remaining > 0; --remaining) {
					this->pop_back();
				}
				return;
			}
			*begin = val; // Reuse existing elements
		}
		// Append new elements
		for (; count > 0; --count) {
			this->_emplace_back(val);
		}
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	void assign(It first, Se last) {
		// Assign range [first, last)
		this->_assign(std::move(first), std::move(last));
	}

	void assign(std::initializer_list<T> initList) {
		// Assign range [initList.begin(), initList.end())
		this->_assign(initList.begin(), initList.end());
	}

	void pop_back() noexcept {
		// Erase the last element
		memory::destruct_at(_data.get_address(_data.index + _data.size - 1));
		if (--_data.size == 0) {
			_data.index = 0;
		}
	}

	void clear() noexcept {
		// Erase all elements
		while (_data.size > 0) {
			this->pop_back();
		}

		// Consider 
		if (_data.map) {
			this->_free_empty_map();
		}
	}

	void resize(const size_type newSize) {
		// Trim or append value-initialized elements to reach newSize
		auto& mySize = _data.size;
		while (newSize > mySize) {
			this->_emplace_back();
		}

		while (newSize < mySize) {
			this->pop_back();
		}
	}

	void resize(const size_type newSize, const T& val) {
		// Trim or append copies of val to reach newSize
		auto& mySize = _data.size;
		while (newSize > mySize) {
			this->_emplace_back(val);
		}

		while (newSize < mySize) {
			this->pop_back();
		}
	}

	void shrink_to_fit() {

	}

	void print_map() const {
		const auto& myMap		= _data.map;
		const auto& myMapSize	= _data.mapSize;
		if (myMapSize == 0) {
			std::cout << "Empty\n";
			return;
		}

		for (auto i = 0; i < myMapSize; ++i) {
			std::cout << " " << i << " ";
		}
		std::cout << "\n";

		for (auto i = 0, j = 0; i < myMapSize; ++i) {
			std::cout << "[";
			if (myMap[i]) {
				std::cout << j;
				++j;
			}
			else {
				std::cout << " ";
			}
			std::cout << "]";
		}
		std::cout << "\nT size: " << sizeof(T) << " - Block size: " << _blockSize << "\n";
	}

private:
	void _construct_n(size_type count) {
		// Construct count value-initialized elements
		_DequeConstructGuard<Deque> guard(this);
		for (; count > 0; --count) {
			this->_emplace_back();
		}
		guard.release();
	}

	void _construct_n(size_type count, const T& val) {
		// Construct count * val
		_DequeConstructGuard<Deque> guard(this);
		for (; count > 0; --count) {
			this->_emplace_back(val);
		}
		guard.release();
	}

	template<class It, class Se>
	void _construct_range(It first, const Se last) {
		// Construct from range [first, last)
		_DequeConstructGuard<Deque> guard(this);
		for (; first != last; ++first) {
			this->_emplace_back(*first);
		}
		guard.release();
	}

	void _grow_map_at_least(const size_type count) {
		// Grow map size by at least count pointers, maintaining circular map structure and order (map size remains power of 2 )
		auto& myMap		= _data.map;
		auto& myMapSize = _data.mapSize;
		// Scale newMapSize to 2^n >= myMapSize + count
		size_type newMapSize = myMapSize > 0 ? myMapSize : 1;
		while (newMapSize - myMapSize < count || newMapSize < _minMapSize) {
			if (newMapSize > this->max_size() / _blockSize - newMapSize) {
				this->_length_error();
			}
			newMapSize *= 2;
		}
		
		const auto blockOffset	= static_cast<size_type>(_data.index / _blockSize);
		const auto blockOffset_	= static_cast<_MapDiffType>(blockOffset);
		const auto myMapSize_	= static_cast<_MapDiffType>(myMapSize);
		// Allocate new map
		const auto newMap = static_cast<_MapPointer>(memory::allocate(newMapSize, sizeof(_BlockPointer)));
		// Copy from the first block to the end of old map to new map
		auto newPtr = memory::uninitialized_copy(myMap + blockOffset_, myMap + myMapSize_, newMap + blockOffset_, newMap + myMapSize_).out;

		const auto mapGrowth	= newMapSize - myMapSize;
		const auto mapGrowth_	= static_cast<_MapDiffType>(mapGrowth);
		if (blockOffset <= mapGrowth) { // Growth is greater than offset of initial block
			// Copy the rest of old map to the right of new copied map
			newPtr = memory::uninitialized_copy(myMap, myMap + blockOffset_, newPtr, newPtr + (mapGrowth_ - blockOffset_)).out;
			// Clear prefix of newMap
			memory::uninitialized_value_construct_n(newMap, blockOffset_);
			// Clear suffix of newMap
			memory::uninitialized_value_construct_n(newPtr, mapGrowth_ - blockOffset_);
		}
		else {
			/*
			This is defensive code for cases in which the "power of 2 map size" policy is broken.

			If oldMapSize is a power of 2, then newMapSize (which is also a power of 2) must be at least 2x oldMapSize.
			This means mapGrowth = newMapSize - oldMapSize is at least oldMapSize	=> mapGrowth >= oldMapSize (1).
			Block offset is guaranteed to be in range [0, oldMapSize)				=> blockOffset < oldMapSize (2).
			From (1) and (2) => mapGrowth > blockOffset.
			
			Therefore, it is theoretically impossible to reach this branch if we follow the "power of 2 map size" policy.
			*/
			// Copy mapGrowth of old map to the right of new copied map
			memory::uninitialized_copy(myMap, myMap + mapGrowth_, newPtr, newPtr + mapGrowth_);
			// Copy the rest of old map wrapped back to the beginning of new map
			newPtr = memory::uninitialized_copy(myMap + mapGrowth_, myMap + blockOffset_, newMap, newMap + (blockOffset_ - mapGrowth_)).out;
			// Clear the middle of new map
			memory::uninitialized_value_construct_n(newPtr, mapGrowth_);
		}

		if (myMap) { // Free old map
			memory::destruct(myMap, myMap + myMapSize_);
			memory::deallocate(myMap, myMapSize * sizeof(_BlockPointer));
		}

		myMap = newMap;
		myMapSize += mapGrowth;
		this->print_map();
	}

	void _free_empty_map() noexcept {
		// Free memory of empty map, assuming each block pointer in map is either nullptr or pointing to a block 
		// without constructed elements
		auto& myMap		= _data.map;
		auto& myMapSize = _data.mapSize;
		
		while (myMapSize > 0) {
			--myMapSize;

			auto& block = myMap[static_cast<_MapDiffType>(myMapSize)];
			if (block) { // Free block
				memory::deallocate(block, _blockSize * sizeof(T));
			}
			memory::destruct_at(std::addressof(block)); // Destruct block pointer
		}

		memory::deallocate(myMap, myMapSize * sizeof(_BlockPointer)); // Free map
		myMap = nullptr;
	}

	template<class... Args>
	void _emplace_front(Args&&... args) {
		// Insert by perfectly forwarding into element at beginning
		auto& myMap		= _data.map;
		auto& myMapSize = _data.mapSize;
		auto& myIndex	= _data.index;
		auto& mySize	= _data.size;

		if (myIndex % _blockSize == 0 && // Insufficient block space for inserting at beginning of the first block
			myMapSize <= (mySize + _blockSize) / _blockSize // Insufficient map space for allocating new block
		) {
			this->_grow_map_at_least(1); // Extend map
		}

		const auto mapCapacity = myMapSize * _blockSize;
		/*
		Adjust myIndex to correctly reflect the first element in deque after map grow.
		
		Normally, to make myIndex wrap around the boundary of the circular structure, we use modulo (%) on mapCapacity.
		However, % is an expensive CPU operation, taking up to hundreds of clock cycles.
		Bitwise & on the other hand is executed directly on the ALU in a single clock cycle.

		Since mapCapacity is guaranteed to be 2^n if myMapSize and _blockSize are also 2^n, we take advantage of the
		performance difference by using this formula:
			X mod 2^n == X & (2^n - 1)
		*/
		myIndex &= mapCapacity - 1; // Equivalent to "myIndex %= mapCapacity;"

		const auto newIndex		= static_cast<size_type>((myIndex != 0 ? myIndex : mapCapacity) - 1);
		const auto blockOffset	= _data.get_block_offset(newIndex);
		if (!myMap[blockOffset]) {
			myMap[blockOffset] = static_cast<_BlockPointer>(memory::allocate(_blockSize, sizeof(T))); // Allocate new block
		}
		// Construct new element by perfect forwarding args
		memory::construct_at(_data.get_address(newIndex), std::forward<Args>(args)...);
		myIndex = newIndex;
		++mySize;
	}

	template<class... Args>
	void _emplace_back(Args&&... args) {
		// Insert by perfectly forwarding into element at end
		auto& myMap		= _data.map;
		auto& myMapSize = _data.mapSize;
		auto& myIndex	= _data.index;
		auto& mySize	= _data.size;

		myIndex &= myMapSize * _blockSize - 1;

		const auto newIndex		= static_cast<size_type>(myIndex + mySize);
		const auto blockOffset	= _data.get_block_offset(newIndex);
		if (!myMap[blockOffset]) {
			std::cout << "Allocate new block at map[" << blockOffset << "]\n";
			myMap[blockOffset] = static_cast<_BlockPointer>(memory::allocate(_blockSize, sizeof(T)));
		}

		memory::construct_at(_data.get_address(newIndex), std::forward<Args>(args)...);
		++mySize;
	}

	template<class It, class Se>
	void _assign(It first, const Se last) {
		// Assign range [first, last)
		const auto end = this->end();
		for (auto begin = this->begin(); begin != end; ++begin, ++first) {
			if (first == last) {
				auto remaining = static_cast<size_type>(end - begin);
				for (; remaining > 0; --remaining) {
					this->pop_back();
				}
				return;
			}
			*begin = *first;
		}
		
		for (; first != last; ++first) {
			this->_emplace_back(*first);
		}
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
#endif // DEQUE_H