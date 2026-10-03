#pragma once
#ifndef DEQUE_H
#define DEQUE_H

#include"compare.hpp"
#include"memory.hpp"

#include<bit>
#include<format>
#include<iostream>

#define USE_MSVC_BLOCK_SIZE

template<class DequeVal>
class _DequeConstIterator {
private:
	using _SizeType = typename DequeVal::size_type;

public:
	using iterator_concept	= std::random_access_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DequeVal::value_type;
	using difference_type	= typename DequeVal::difference_type;
	using pointer			= typename DequeVal::const_pointer;
	using reference			= const value_type&;

	_DequeConstIterator() noexcept
		: offset(0), data()  {}

	_DequeConstIterator(const _SizeType offset, const DequeVal* data) noexcept
		: offset(offset), data(data) {}

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
	using _BaseIter::_BaseIter;

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

	static constexpr std::size_t _bytes	= sizeof(value_type);
	
#ifndef USE_MSVC_BLOCK_SIZE
	// Each block can hold up to 256 bytes
	static constexpr std::size_t _maxBlockBytes = 256;
	
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
		_rawBlockSize	= 256 / 13
						= 19;
		_blockSize		= std::bit_floor(19)
						= 2^4
						= 16 (<= 19);

		=> each block is 16 * 13 = 206 bytes.

	If element size exceeds 128 bytes, each block will accommodate only 1 single element.
	In this case, Deque degrades in to a structure with similar behaviour to a List/ForwardList, while retaining
	redundant overhead, resulting in lower efficiency and performance.

	Therefore, it is strongly recommended to opt for a List/ForwardList instead of Deque when dealing with types
	larger than this threshold.
	*/
	static constexpr std::size_t _rawBlockSize = (_bytes < _maxBlockBytes) ? _maxBlockBytes / _bytes : 1;
#endif // !USE_MSVC_BLOCK_SIZE

public:
	// Number of elements per block, power of 2, scale with element size
#ifdef USE_MSVC_BLOCK_SIZE
	// According to MSVC, each block can hold up to 16 elements.
	static constexpr std::size_t _blockSize = _bytes <= 1 ? 16
											: _bytes <= 2 ? 8
											: _bytes <= 4 ? 4
											: _bytes <= 8 ? 2
											:				1;
#else
	static constexpr std::size_t _blockSize = std::bit_floor(_rawBlockSize);
#endif // USE_MSVC_BLOCK_SIZE

	_DequeValue() noexcept
		: map(), mapSize(0), index(0), size(0) {}

	difference_type get_block_offset(const size_type index) const noexcept {
		// Get block offset in map from element index
		return static_cast<difference_type>((index / _blockSize) & (mapSize - 1));
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
};

template<class Container>
struct _DequeConstructGuard {
	// Guard for deque construction failure
	_DequeConstructGuard(Container* target)
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

	Container* target;
};

enum class _GrowthDirection : bool {
	FRONT,
	BACK
};

template<class Container, _GrowthDirection _direction>
struct _DequeInsertGuard {
	// Guard for deque insertion failure
	using size_type = typename Container::size_type;

	_DequeInsertGuard(Container* target, const size_type oldSize)
		: target(target), _oldSize(oldSize) {}

	_DequeInsertGuard(const _DequeInsertGuard&)				= delete;
	_DequeInsertGuard& operator=(const _DequeInsertGuard&)	= delete;

	~_DequeInsertGuard() noexcept {
		if (target) {
			while (_oldSize < target->_data.size) {
				if constexpr (_direction == _GrowthDirection::FRONT) {
					target->pop_front();
				}
				else {
					target->pop_back();
				}
			}
		}
	}

	void release() noexcept {
		target = nullptr;
	}
	
	Container* target;

private:
	const size_type _oldSize; // Number of elements to restore
};

template<class T>
class Deque {
private:
	template<class Container, _GrowthDirection _direction>
	friend struct _DequeInsertGuard;

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

	using _MyVal = _DequeValue<value_type, size_type, difference_type, pointer, const_pointer, _MapPointer>;

	static constexpr size_type _minMapSize	= 8;
	static constexpr size_type _blockSize	= _MyVal::_blockSize;

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
			this->_assign_range(other.begin(), other.end());
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
		this->_assign_range(initList.begin(), initList.end());
		return *this;
	}

	[[nodiscard]] T& operator[](const size_type index) noexcept {
		return this->_subscript(index); // UB: nullptr dereference
	}

	[[nodiscard]] const T& operator[](const size_type index) const noexcept {
		return this->_subscript(index);
	}

	[[nodiscard]] T& at(const size_type index) {
		if (index >= _data.size) {
			this->_subscription_error();
		}
		return this->_subscript(index);
	}

	[[nodiscard]] const T& at(const size_type index) const {
		if (index >= _data.size) {
			this->_subscription_error();
		}
		return this->_subscript(index);
	}

	[[nodiscard]] iterator begin() noexcept {
		return iterator(_data.index, std::addressof(_data));
	}

	[[nodiscard]] const_iterator begin() const noexcept {
		return const_iterator(_data.index, std::addressof(_data));
	}

	[[nodiscard]] iterator end() noexcept {
		return iterator(_data.index + _data.size, std::addressof(_data));
	}

	[[nodiscard]] const_iterator end() const noexcept {
		return const_iterator(_data.index + _data.size, std::addressof(_data));
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
		return this->_subscript(0); // UB: nullptr dereference
	}

	[[nodiscard]] const T& front() const noexcept {
		return this->_subscript(0);
	}

	[[nodiscard]] T& back() noexcept {
		return this->_subscript(_data.size - 1); // UB: nullptr dereference
	}

	[[nodiscard]] const T& back() const noexcept {
		return this->_subscript(_data.size - 1);
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

	[[nodiscard]] size_type map_size() const noexcept {
		return _data.mapSize;
	}

	[[nodiscard]] size_type block_size() const noexcept {
		return _blockSize;
	}

	template<class... Args>
	iterator emplace(const_iterator where, Args&&... args) {
		// Insert by perfectly forwarding args at where
		const auto offset = static_cast<size_type>(where - this->begin());
		if (offset == 0) { // Insert at beginning
			this->_emplace_front(std::forward<Args>(args)...);
		}
		else if (offset == _data.size) { // Insert at end
			this->_emplace_back(std::forward<Args>(args)...);
		}
		else {
			// Optimize to shift as few elements as possible by constructing at front or back, whichever is closer to offset
			memory::_TempObjectGuard<T> object(std::forward<Args>(args)...);
			if (offset <= _data.size / 2) { // Insert closer to front, shift elements to the left
				// Construct by moving the first element before begin()
				this->_emplace_front(std::move(*this->begin()));
				
				/*
				After _emplace_front(), begin() points to the new first element.
				Let oldBegin = begin() before _emplace_front().
				Let newBegin = begin() after _emplace_front() = oldBegin - 1.
				
				Shift range [oldBegin + 1, oldBegin + offset) or range [newBegin + 2, newBegin + 1 + offset)
				to the left by 1 offset.
				*/
				const auto newBegin	= this->begin();
				const auto newWhere	= newBegin + offset;

				memory::move(newBegin + 2, newWhere + 1, newBegin + 1);
				*newWhere = std::move(object.get_value());
			}
			else { // Insert closer to back, shift elements to the right
				// Construct by moving the last element at end()
				this->_emplace_back(std::move(*(this->end() - 1)));

				const auto newEnd	= this->end();
				const auto newWhere	= this->begin() + offset;
				
				memory::move_backward(newWhere, newEnd - 2, newEnd - 1);
				*newWhere = std::move(object.get_value());
			}
		}
		return this->begin() + offset;
	}

	template<class... Args>
	reference emplace_front(Args&&... args) {
		// Insert by perfectly forwarding args at beginning
		this->_emplace_front(std::forward<Args>(args)...);
		return this->front();
	}

	template<class... Args>
	reference emplace_back(Args&&... args) {
		// Insert by perfectly forwarding args at end
		this->_emplace_back(std::forward<Args>(args)...);
		return this->back();
	}

	void push_front(const T& val) {
		// Insert val at beginning
		this->_emplace_front(val);
	}

	void push_front(T&& val) {
		// Insert val at beginning
		this->_emplace_front(std::move(val));
	}

	void push_back(const T& val) {
		// Insert val at end
		this->_emplace_back(val);
	}

	void push_back(T&& val) {
		// Insert val at end
		this->_emplace_back(std::move(val));
	}

	iterator insert(const_iterator where, const T& val) {
		// Insert by copying val at where
		return this->emplace(where, val);
	}

	iterator insert(const_iterator where, T&& val) {
		// Insert by moving val at where
		return this->emplace(where, std::move(val));
	}

	iterator insert(const_iterator where, const size_type count, const T& val) {
		// Insert count * val at where
		if (count == 0) {
			return;
		}

		const auto offset = static_cast<size_type>(where - this->begin());

		const auto oldSize		= _data.size;
		const auto remaining	= oldSize - offset;
		if (offset < remaining) { // Insert closer to front
			_DequeInsertGuard<Deque, _GrowthDirection::FRONT> guard(this, oldSize);
			if (offset < count) { // Insert longer than prefix
				// Push excessive elements
				for (auto _i = count - offset; _i > 0; --_i) {
					this->_emplace_front(val);
				}
				// Push prefix
				for (auto _i = offset; _i > 0; --_i) {
					this->_emplace_front(this->_subscript(count - 1));
				}
				// Fill remaining values
				memory::fill_n(this->begin() + count, offset, val);
			}
			else { // Insert shorter than prefix
				// Push part of prefix
				for (auto _i = count; _i > 0; --_i) {
					this->_emplace_front(this->_subscript(count - 1));
				}

				memory::_TempObjectGuard<T> object(val);

				const auto mid = this->begin() + count;
				memory::move(mid + count, mid + offset, mid); // Move the rest of prefix
				memory::fill(this->begin() + offset, mid + offset, object.get_value()); // Fill remaining values
			}
			guard.release();
		}
		else { // Insert closer to back
			_DequeInsertGuard<Deque, _GrowthDirection::BACK> guard(this, oldSize);
			if (remaining < count) { // Insert longer than suffix
				// Push excessive elements
				for (auto _i = count - remaining; _i > 0; --_i) {
					this->_emplace_back(val);
				}
				// Push suffix
				for (size_type _i = 0; _i < remaining; ++_i) {
					this->_emplace_back(this->_subscript(offset + _i));
				}
				// Fill remaining values
				memory::fill_n(this->begin() + offset, remaining, val);
			}
			else { // Insert shorter than suffix
				// Push part of suffix
				for (size_type _i = 0; _i < count; ++_i) {
					this->_emplace_back(this->_subscript(offset + remaining - count + _i));
				}

				memory::_TempObjectGuard<T> object(val);

				const auto mid = this->begin() + offset;
				memory::move_backward(mid, mid + remaining - count, mid + remaining); // Move the rest of suffix
				memory::fill_n(mid, count, object.get_value()); // Fill remaining values
			}
			guard.release();
		}
		return this->begin() + offset;
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	iterator insert(const_iterator where, It first, Se last) {
		// Insert range [first, last) at where
		return this->_insert_range(where, first, last);
	}

	iterator insert(const_iterator where, std::initializer_list<T> initList) {
		// Insert initList at where
		return this->_insert_range(where, initList.begin(), initList.end());
	}

	iterator prepend(const size_type count, const T& val) {
		// Prepend count * val
		_DequeInsertGuard<Deque, _GrowthDirection::FRONT> guard(this, _data.size);
		for (; count > 0; --count) {
			this->_emplace_front(val);
		}
		guard.release();
		return this->begin();
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	iterator prepend(It first, Se last) {
		// Prepend range [first, last)
		_DequeInsertGuard<Deque, _GrowthDirection::FRONT> guard(this, _data.size);
		if constexpr (std::bidirectional_iterator<It>) {
			if constexpr (std::same_as<It, Se>) {
				while (first != last) {
					this->_emplace_front(*--last);
				}
			}
			else {
				auto end = std::ranges::next(first, last);
				while (first != end) {
					this->_emplace_front(*--end);
				}
			}
		}
		else {
			size_type count = 0;
			for (; first != last; ++first, ++count) {
				this->_emplace_front(*first);
			}

			std::reverse(this->begin(), this->begin() + count);
		}
		guard.release();
		return this->begin();
	}

	iterator prepend(std::initializer_list<T> initList) {
		// Prepend initList
		return this->prepend(initList.begin(), initList.end());
	}

	iterator append(const size_type count, const T& val) {
		// Append count * val
		const auto oldSize = _data.size;

		_DequeInsertGuard<Deque, _GrowthDirection::BACK> guard(this, oldSize);
		for (; count > 0; --count) {
			this->_emplace_back(val);
		}
		guard.release();
		return this->begin() + oldSize;
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	iterator append(It first, const Se last) {
		// Append range [first, last)
		const auto oldSize = _data.size;

		_DequeInsertGuard<Deque, _GrowthDirection::BACK> guard(this, oldSize);
		for (; first != last; ++first) {
			this->_emplace_back(*first);
		}
		guard.release();
		return this->begin() + oldSize;
	}

	iterator append(std::initializer_list<T> initList) {
		// Append initList
		return this->append(initList.begin(), initList.end());
	}

	void assign(const size_type count) {
		// Assign count * value-initialized elements
		this->_assign(count);
	}

	void assign(const size_type count, const T& val) {
		// Assign count * val
		this->_assign(count, val);
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	void assign(It first, Se last) {
		// Assign range [first, last)
		this->_assign_range(std::move(first), std::move(last));
	}

	void assign(std::initializer_list<T> initList) {
		// Assign initList
		this->_assign_range(initList.begin(), initList.end());
	}

	void pop_front() noexcept {
		// Erase the first element
		memory::destruct_at(_data.get_address(_data.index));
		if (--_data.size == 0) {
			_data.index = 0;
		}
		else {
			++_data.index;
		}
	}

	void pop_back() noexcept {
		// Erase the last element
		memory::destruct_at(_data.get_address(_data.index + _data.size - 1));
		if (--_data.size == 0) {
			_data.index = 0;
		}
	}

	iterator erase(const_iterator where)
		noexcept(std::is_nothrow_move_assignable_v<T>)
	{
		// Erase element at where
		return this->erase(where, where + 1);
	}

	iterator erase(const_iterator first, const_iterator last)
		noexcept(std::is_nothrow_move_assignable_v<T>)
	{
		// Erase range [first, last)
		const auto offset = static_cast<size_type>(first - this->begin());
		
		auto count = static_cast<size_type>(last - first);
		if (count != 0) {
			const auto begin	= this->begin() + offset;
			const auto end		= begin + count;

			if (offset < _data.size / 2) {
				memory::move_backward(this->begin(), begin, end);
				for (; count > 0; --count) {
					this->pop_front();
				}
			}
			else {
				memory::move(end, this->end(), begin);
				for (; count > 0; --count) {
					this->pop_back();
				}
			}
		}
		return this->begin() + offset;
	}

	void clear() noexcept {
		// Erase all elements and free all memory
		while (_data.size > 0) {
			this->pop_back();
		}

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
		// Shrink capacity to size
		auto& myMap		= _data.map;
		auto& myMapSize = _data.mapSize;
		auto& myIndex	= _data.index;
		auto& mySize	= _data.size;

		if (mySize == 0) {
			if (myMap) {
				this->_free_empty_map();
			}
			return;
		}

		/*
		This is the same idea as in _emplace_front()/_emplace_back().
		We use (myMapSize - 1) as a mask for bitwise AND to wrap around the circular map, instead of using modulo.
		*/
		const auto mask = static_cast<size_type>(myMapSize - 1);

		const auto unmaskedFirstUsedBlockOffset = static_cast<size_type>(myIndex / _blockSize);
		/*
		Calculate (myIndex + mySize - 1) to get the index of the last element
		Divide by _blockSize to get the unmasked offset of the last used block
		Add 1 to get the unmasked offset of the first unused block
		*/
		const auto unmaskedFirstUnusedBlockOffset	= static_cast<size_type>(((myIndex + mySize - 1) / _blockSize) + 1);

		const auto firstUsedBlockOffset		= static_cast<size_type>(unmaskedFirstUsedBlockOffset & mask);
		const auto firstUnusedBlockOffset	= static_cast<size_type>(unmaskedFirstUnusedBlockOffset & mask);
		// Deallocate unused blocks, traversing over the circular map until reaching the first used block offset
		for (auto blockOffset = firstUnusedBlockOffset;;) {
			if (blockOffset == firstUsedBlockOffset) {
				break;
			}

			auto& block = myMap[blockOffset];
			if (block) {
				memory::deallocate(block, _blockSize * sizeof(T));
				block = nullptr;
			}

			blockOffset = static_cast<size_type>((blockOffset + 1) & mask);
		}

		const auto usedBlockCount = static_cast<size_type>(unmaskedFirstUnusedBlockOffset - unmaskedFirstUsedBlockOffset);
		// Shrink map size to newMapSize, power of 2, maintaining circular map structure and order
		auto newMapSize = _minMapSize;
		while (newMapSize < usedBlockCount) {
			newMapSize *= 2;
		}

		if (newMapSize >= myMapSize) {
			return;
		}

		const auto newMap = static_cast<_MapPointer>(memory::allocate(newMapSize, sizeof(_BlockPointer)));
		// Transfer ownership of old blocks to new map
		for (size_type blockOffset = 0; blockOffset < usedBlockCount; ++blockOffset) {
			const auto oldBlockOffset = static_cast<size_type>((firstUsedBlockOffset + blockOffset) & mask);
			memory::construct_at(newMap + blockOffset, myMap[oldBlockOffset]);
		}
		// Clear the rest of new map
		memory::uninitialized_value_construct_n(newMap + usedBlockCount, newMapSize - usedBlockCount);
		// Free old map
		for (auto blockOffset = myMapSize; blockOffset > 0;) {
			--blockOffset;
			memory::destruct_at(myMap + blockOffset);
		}
		memory::deallocate(myMap, myMapSize * sizeof(_BlockPointer));

		myMap		= newMap;
		myMapSize	= newMapSize;
		myIndex		%= _blockSize;
	}

	void print_map() const {
		const auto& myMap		= _data.map;
		const auto& myMapSize	= _data.mapSize;
		if (myMapSize == 0) {
			std::cout << "Empty\n";
			return;
		}

		const auto firstBlock = _data.get_block_offset(_data.index);
		std::vector<std::string> blocks(myMapSize);

		auto i = firstBlock;
		auto j = 0;
		for (i = firstBlock; i < myMapSize; ++i) {
			if (_data.map[i]) {
				blocks[i] = std::format("{}", j++);
			}
			else {
				blocks[i] = " ";
			}
		}
		for (i = 0; i < firstBlock; ++i) {
			if (_data.map[i]) {
				blocks[i] = std::format("{}", j++);
			}
			else {
				blocks[i] = " ";
			}
		}

		for (i = 0; i < myMapSize; ++i) {
			std::cout << " " << i << " ";
		}
		std::cout << "\n";

		for (i = 0; i < myMapSize; ++i) {
			std::cout << "[" << blocks[i] << "]";
		}
		std::cout
			<< "\nT size:         " << sizeof(T)
			<< "\nBlock size:     " << _blockSize
			<< "\nMap size:       " << _data.mapSize
			<< "\nDeque size:     " << _data.size
			<< "\nIndex in map:   " << _data.index
			<< "\nIndex in block: " << _data.index % _blockSize << "\n";
	}

private:
	template<class... Args>
	void _construct_n(size_type count, const Args&... args) {
		// Construct deque with count elements constructed from args
		_DequeConstructGuard<Deque> guard(this);
		for (; count > 0; --count) {
			this->_emplace_back(args...);
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

	reference _subscript(const size_type index) noexcept {
		// Get element at index
		return _data.subscript(_data.index + index);
	}

	const_reference _subscript(const size_type index) const noexcept {
		// Get element at index
		return _data.subscript(_data.index + index);
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
		
		const auto blockOffset = _data.index / _blockSize;
		// Allocate new map
		const auto newMap = static_cast<_MapPointer>(memory::allocate(newMapSize, sizeof(_BlockPointer)));
		// Copy from the first block to the end of old map to new map
		auto constructedLast = memory::uninitialized_copy(myMap + blockOffset, myMap + myMapSize, newMap + blockOffset, newMap + myMapSize).out;

		const auto mapGrowth = newMapSize - myMapSize;
		if (blockOffset <= mapGrowth) { // Growth is greater than offset of initial block
			// Copy the rest of old map to the right of new copied map
			constructedLast = memory::uninitialized_copy(
				myMap, myMap + blockOffset, constructedLast, constructedLast + blockOffset
			).out;
			// Clear prefix of newMap
			memory::uninitialized_value_construct_n(newMap, blockOffset);
			// Clear suffix of newMap
			memory::uninitialized_value_construct_n(constructedLast, mapGrowth - blockOffset);
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
			memory::uninitialized_copy(myMap, myMap + mapGrowth, constructedLast, constructedLast + mapGrowth);
			// Copy the rest of old map wrapped back to the beginning of new map
			constructedLast = memory::uninitialized_copy(
				myMap + mapGrowth, myMap + blockOffset, newMap, newMap + (blockOffset - mapGrowth)
			).out;
			// Clear the middle of new map
			memory::uninitialized_value_construct_n(constructedLast, mapGrowth);
		}

		if (myMap) { // Free old map
			memory::destruct(myMap, myMap + myMapSize);
			memory::deallocate(myMap, myMapSize * sizeof(_BlockPointer));
		}

		myMap		= newMap;
		myMapSize	+= mapGrowth;
	}

	void _free_empty_map() noexcept {
		// Free memory of empty map, assuming each block pointer in map is either nullptr or pointing to a block 
		// with no constructed elements
		auto& myMap		= _data.map;
		auto& myMapSize = _data.mapSize;
		auto& myIndex	= _data.index;
		// Free each block in map
		const auto oldMapSize = myMapSize;
		while (myMapSize > 0) {
			--myMapSize;

			auto& block = myMap[myMapSize];
			if (block) {
				memory::deallocate(block, _blockSize * sizeof(T));
			}
			memory::destruct_at(std::addressof(block));
		}
		// Free map
		memory::deallocate(myMap, oldMapSize * sizeof(_BlockPointer));
		
		myMap		= nullptr;
		myMapSize	= 0;
		myIndex		= 0;
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
			this->_grow_map_at_least(1);
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
		
		/*
		This is MSVC's std::deque initial intention: on the first insertion into an empty deque, it places the
		element at the end of the block furthest from the intended growth direction. Specifically, emplace_front()
		chooses offset _blockSize - 1, while emplace_back() chooses offset 0. This leaves maximum space for as many
		consecutive calls in the same direction as possible before allocating a new block is needed.

		The trade-off: An emplace_front() immediately followed by emplace_back() (or vice versa) forces an extra
		block allocation, since the two elements end up in opposite ends of the block with nothing in between.
		Allocating 2 separate blocks to hold just 2 elements inserted in opposite directions is highly inefficient.

		We avoid this by centering the first element in the block, leaving room on both sides for subsequent insertions
		regardless of growth direction.
		*/
		if (mySize == 0) {
			myIndex = _blockSize / 2;
		}

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

		if ((myIndex + mySize) % _blockSize == 0 &&
			myMapSize <= (mySize + _blockSize) / _blockSize
		) {
			this->_grow_map_at_least(1);
		}

		myIndex &= myMapSize * _blockSize - 1;

		if (mySize == 0) {
			myIndex = _blockSize / 2;
		}

		const auto newIndex		= static_cast<size_type>(myIndex + mySize);
		const auto blockOffset	= _data.get_block_offset(newIndex);
		if (!myMap[blockOffset]) {
			myMap[blockOffset] = static_cast<_BlockPointer>(memory::allocate(_blockSize, sizeof(T)));
		}

		memory::construct_at(_data.get_address(newIndex), std::forward<Args>(args)...);
		++mySize;
	}

	template<class It, class Se>
	void _insert_uncounted_range(const size_type offset, It first, Se last) {
		// Insert unknown number of elements from [first, last) at offset
		if (first == last) {
			return;
		}

		/*
		For uncounted range, the general strategy is to insert all elements in range [first, last) at front or back,
		whichever is closer to offset, then rotate the inserted elements to the correct position.
		*/
		const auto oldSize		= _data.size;
		const auto remaining	= oldSize - offset;
		if (offset < remaining) {
			// Uncounted range is not bidirectional, so we must iterate forwards and reverse the constructed range
			// when inserting at front to maintain elements order.
			_DequeInsertGuard<Deque, _GrowthDirection::FRONT> guard(this, oldSize);
			for (; first != last; ++first) {
				this->_emplace_front(*first);
			}
			guard.release();

			const auto mid = this->begin() + static_cast<size_type>(_data.size - oldSize);
			std::reverse(this->begin(), mid);

			std::rotate(this->begin(), mid, mid + offset);
		}
		else {
			_DequeInsertGuard<Deque, _GrowthDirection::BACK> guard(this, oldSize);
			for (; first != last; ++first) {
				this->_emplace_back(*first);
			}
			guard.release();

			std::rotate(this->begin() + offset, this->begin() + oldSize, this->end());
		}
	}

	template<class It, class Se>
	void _insert_counted_range(const size_type offset, const size_type count, It first, const Se last) {
		// Insert elements from counted range [first, first + count) at offset
		if (count == 0) {
			return;
		}

		const auto oldSize		= _data.size;
		const auto remaining	= oldSize - offset;
		if (offset < remaining) {
			_DequeInsertGuard<Deque, _GrowthDirection::FRONT> guard(this, oldSize);
			if (offset < count) {
				/*
				If [first, last) is a bidirectional range, we can iterate backwards and construct to the front of *this,
				maintaining elements order.

				Otherwise, we have to iterate forwards and construct as usual, then reverse the constructed range.
				*/
				if constexpr (std::bidirectional_iterator<It>) {
					const auto tail = std::ranges::next(first, count - offset);
					for (auto mid = tail; mid != first;) {
						this->_emplace_front(*--mid);
					}
					first = tail;
				}
				else {
					for (auto _i = count - offset; _i > 0; --_i, ++first) {
						this->_emplace_front(*first);
					}
					std::reverse(this->begin(), this->begin() + (count - offset));
				}

				for (auto _i = offset; _i > 0; --_i) {
					this->_emplace_front(this->_subscript(count - 1));
				}

				memory::copy_n(first, offset, this->begin() + count);
			}
			else {
				for (auto _i = count; _i > 0; --_i) {
					this->_emplace_front(this->_subscript(count - 1));
				}

				const auto mid = this->begin() + count;
				memory::move(mid + count, mid + offset, mid);
				memory::copy_n(first, count, this->begin() + offset);
			}
			guard.release();
		}
		else {
			_DequeInsertGuard<Deque, _GrowthDirection::BACK> guard(this, oldSize);
			if (remaining < count) {
				for (auto mid = std::ranges::next(first, remaining); mid != last; ++mid) {
					this->_emplace_back(*mid);
				}

				for (size_type _i = 0; _i < remaining; ++_i) {
					this->_emplace_back(this->_subscript(offset + _i));
				}

				memory::copy_n(first, remaining, this->begin() + offset);
			}
			else {
				for (auto _i = count; _i > 0; --_i) {
					this->_emplace_back(this->_subscript(offset + remaining - _i));
				}

				const auto mid = this->begin() + offset;
				memory::move_backward(mid, mid + remaining - count, mid + remaining);
				memory::copy_n(first, count, mid);
			}
			guard.release();
		}
	}

	template<class It, class Se>
	iterator _insert_range(const_iterator where, It first, Se last) {
		// Insert range [first, last) at where
		const auto offset = static_cast<size_type>(where - this->begin());
		if constexpr (std::forward_iterator<It>) {
			const auto count = static_cast<size_type>(std::ranges::distance(first, last));
			this->_insert_counted_range(offset, count, std::move(first), std::move(last));
		}
		else {
			this->_insert_uncounted_range(offset, std::move(first), std::move(last));
		}
		return this->begin() + offset;
	}

	template<class... Args>
	void _assign(size_type count, const Args&... args) {
		// Assign count elements constructed from args
		const auto end = this->end();
		for (auto begin = this->begin(); begin != end; ++begin, --count) {
			if (count == 0) { // Trim excessive elements
				auto remaining = static_cast<size_type>(end - begin);
				for (; remaining > 0; --remaining) {
					this->pop_back();
				}
				return;
			}
			// Reuse existing elements
			if constexpr (sizeof...(args) == 0) {
				*begin = T{};
			}
			else{
				((*begin = args), ...);
			}
		}
		// Append new elements
		for (; count > 0; --count) {
			this->_emplace_back(args...);
		}
	}

	template<class It, class Se>
	void _assign_range(It first, const Se last) {
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

template<class T>
constexpr void swap(Deque<T>& lhs, Deque<T>& rhs) noexcept {
	lhs.swap(rhs);
}

template<class T>
[[nodiscard]] constexpr bool operator==(const Deque<T>& lhs, const Deque<T>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T>
[[nodiscard]] constexpr compare::SynthThreeWayCompareResult<T> operator<=>(
	const Deque<T>& lhs, const Deque<T>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // DEQUE_H