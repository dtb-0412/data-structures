#pragma once
#ifndef DEQUE_H
#define DEQUE_H

#include"compare.hpp"
#include"memory.hpp"
#include<iostream>

template<class DequeVal>
class _DequeConstIterator {
private:
	using _SizeType = typename DequeVal::size_type;

	static constexpr int _blockSize = DequeVal::_blockSize;

public:
	using iterator_concept	= std::random_access_iterator_tag;
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename DequeVal::value_type;
	using difference_type	= typename DequeVal::difference_type;
	using pointer			= typename DequeVal::pointer;
	using reference			= const value_type&;

	_DequeConstIterator() noexcept
		: data(), offset(0) {}

	_DequeConstIterator(DequeVal* data, const _SizeType offset) noexcept
		: data(data), offset(offset) {}

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
	DequeVal* data;
	_SizeType offset;
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
	using _MapPointer			= MapPtr;
	using _MapDifferenceType	= typename std::iterator_traits<MapPtr>::difference_type;

	static constexpr std::size_t _bytes = sizeof(value_type);

public:
	// Number of elements per block, scale with element size (power of 2)
	static constexpr int _blockSize = _bytes <= 1 ? 16
									: _bytes <= 2 ? 8
									: _bytes <= 4 ? 4
									: _bytes <= 8 ? 2
									:				1;

	_DequeValue() noexcept
		: map(), mapSize(0), offset(0), size(0) {}

	_MapDifferenceType get_block(const size_type offset) const noexcept {
		return static_cast<_MapDifferenceType>((offset / _blockSize) & (mapSize - 1));
	}

	reference subscript(const size_type offset) noexcept {
		const auto block		= this->get_block(offset);
		const auto blockOffset	= static_cast<difference_type>(offset % _blockSize);
		return map[block][blockOffset];
	}

	const_reference subscript(const size_type offset) const noexcept {
		const auto block = this->get_block(offset);
		const auto blockOffset = static_cast<difference_type>(offset % _blockSize);
		return map[block][blockOffset];
	}

	value_type* address_subscript(const size_type offset) noexcept {
		const auto block = this->get_block(offset);
		const auto blockOffset = static_cast<difference_type>(offset % _blockSize);
		return map[block] + blockOffset;
	}

	_MapPointer map;	// Pointer to array of pointers to blocks
	
	size_type mapSize;	// Size of map array
	size_type offset;	// Offset of initial element
	size_type size;		// Number of elements
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
	using _MapPointer			= T**;
	using _MapDifferenceType	= typename std::iterator_traits<_MapPointer>::difference_type;

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

private:
	_MyVal _data;
};
#endif // DEQUE_H