#pragma once
#ifndef DYNAMIC_ARRAY_H
#define DYNAMIC_ARRAY_H

#include"memory.hpp"
#include"type_traits.hpp"
#include"utility.hpp"

template<class _DynamicArrVal>
class _DynamicArrayConstIterator {
public:
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename _DynamicArrVal::ValueType;
	using difference_type	= typename _DynamicArrVal::DifferenceType;
	using pointer			= typename _DynamicArrVal::ConstPointer;
	using reference			= const value_type&;

private:
	using _Pointer = typename _DynamicArrVal::Pointer;

public:
	_DynamicArrayConstIterator() noexcept
		: ptr() {}

	_DynamicArrayConstIterator(_Pointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] reference operator*() const noexcept {
		return *ptr; // UB: nullptr or end() dereference
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return ptr; // UB: nullptr or end() dereference
	}

	_DynamicArrayConstIterator& operator++() noexcept {
		++ptr;  // UB: increment past end()
		return *this;
	}

	_DynamicArrayConstIterator operator++(int) noexcept {
		_DynamicArrayConstIterator temp = *this;
		++(*this);
		return temp;
	}

	_DynamicArrayConstIterator& operator--() noexcept {
		--ptr;  // UB: decrement past begin()
		return *this;
	}

	_DynamicArrayConstIterator operator--(int) noexcept {
		_DynamicArrayConstIterator temp = *this;
		--(*this);
		return temp;
	}

	_DynamicArrayConstIterator& operator+=(const difference_type offset) noexcept {
		ptr += offset;  // UB: increment past end()
		return *this;
	}

	[[nodiscard]] _DynamicArrayConstIterator operator+(const difference_type offset) const noexcept {
		_DynamicArrayConstIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] friend _DynamicArrayConstIterator operator+(const difference_type offset, _DynamicArrayConstIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	_DynamicArrayConstIterator& operator-=(const difference_type offset) noexcept {
		ptr -= offset;  // UB: decrement past begin()
		return *this;
	}

	[[nodiscard]] _DynamicArrayConstIterator operator-(const difference_type offset) const noexcept {
		_DynamicArrayConstIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] difference_type operator-(const _DynamicArrayConstIterator& other) const noexcept {
		return static_cast<difference_type>(ptr - other.ptr);  // UB: 2 iterators don't belong to the same container
	}

	[[nodiscard]] reference operator[](const difference_type offset) const noexcept {
		return *(*this + offset);  // UB: nullptr or end() dereference
	}

	[[nodiscard]] bool operator==(const _DynamicArrayConstIterator& other) const noexcept {
		return ptr == other.ptr;  // UB: iterators don't belong to the same container
	}

	[[nodiscard]] bool operator!=(const _DynamicArrayConstIterator& other) const noexcept {
		return !(*this == other);
	}

	[[nodiscard]] bool operator<(const _DynamicArrayConstIterator& other) const noexcept {
		return ptr < other.ptr;  // UB: iterators don't belong to the same container
	}

	[[nodiscard]] bool operator>(const _DynamicArrayConstIterator& other) const noexcept {
		return other < *this;
	}

	[[nodiscard]] bool operator<=(const _DynamicArrayConstIterator& other) const noexcept {
		return !(other < *this);
	}

	[[nodiscard]] bool operator>=(const _DynamicArrayConstIterator& other) const noexcept {
		return !(*this < other);
	}

	_Pointer ptr;
};

template<class _DynamicArrVal>
class _DynamicArrayIterator : public _DynamicArrayConstIterator<_DynamicArrVal> {
private:
	using _BaseIter = _DynamicArrayConstIterator<_DynamicArrVal>;
	using _BaseIter::_BaseIter;  // Inherit _BaseIter's constructors

public:
	using iterator_category = std::random_access_iterator_tag;
	using value_type		= typename _DynamicArrVal::ValueType;
	using difference_type	= typename _DynamicArrVal::DifferenceType;
	using pointer			= typename _DynamicArrVal::Pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return this->ptr;
	}

	_DynamicArrayIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	_DynamicArrayIterator operator++(int) noexcept {
		_DynamicArrayIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	_DynamicArrayIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	_DynamicArrayIterator operator--(int) noexcept {
		_DynamicArrayIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}

	_DynamicArrayIterator& operator+=(const difference_type offset) noexcept {
		_BaseIter::operator+=(offset);
		return *this;
	}

	[[nodiscard]] _DynamicArrayIterator operator+(const difference_type offset) const noexcept {
		_DynamicArrayIterator temp = *this;
		temp += offset;
		return temp;
	}

	[[nodiscard]] friend _DynamicArrayIterator operator+(const difference_type offset, _DynamicArrayIterator iter) noexcept {
		iter += offset;
		return iter;
	}

	_DynamicArrayIterator& operator-=(const difference_type offset) noexcept {
		_BaseIter::operator-=(offset);
		return *this;
	}

	// There are 2 overloads of _BaseIter::operator-(). If we override any one of them, the others will be hidden by default.
	// In this case, we only need to override the operator-(const difference_type).
	// Therefore we explicitly announce that we want to inherit all other overloads except for the ones being overridden.
	using _BaseIter::operator-;

	[[nodiscard]] _DynamicArrayIterator operator-(const difference_type offset) const noexcept {
		_DynamicArrayIterator temp = *this;
		temp -= offset;
		return temp;
	}

	[[nodiscard]] reference operator[](const difference_type offset) const noexcept {
		return const_cast<reference>(_BaseIter::operator[](offset));
	}
};

template<class _ValueType, class _SizeType, class _DiffType, class _Pointer, class _ConstPointer>
class _DynamicArrValue {
public:
	using ValueType			= _ValueType;
	using SizeType			= _SizeType;
	using DifferenceType	= _DiffType;
	using Pointer			= _Pointer;
	using ConstPointer		= _ConstPointer;
	using Reference			= ValueType&;
	using ConstReference	= const ValueType&;

	_DynamicArrValue() noexcept
		: first(), last(), end() {}

	_DynamicArrValue(_Pointer first, _Pointer last, _Pointer end) noexcept
		: first(first), last(last), end(end) {}

	void swap(_DynamicArrValue& Other) noexcept {
		// Swap contents with Other (ADL)
		swap(first, Other.first);
		swap(last, Other.last);
		swap(end, Other.end);
	}

	void takeOwnership(_DynamicArrValue& Other) noexcept {
		// Take ownership of Other's contents, leaving Other empty
		first	= std::exchange(Other.first, nullptr);
		last	= std::exchange(Other.last, nullptr);
		end		= std::exchange(Other.end, nullptr);
	}

public:
	_Pointer first; // Points to the beginning of the array
	_Pointer last;	// Points to the end of values in the array (size)
	_Pointer end;	// Points to the end of allocated memory in the array (capacity)
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
	using _DynamicArrVal = _DynamicArrValue<value_type, size_type, difference_type, pointer, const_pointer>;

public:
	using iterator			= _DynamicArrayIterator<_DynamicArrVal>;
	using const_iterator	= _DynamicArrayConstIterator<_DynamicArrVal>;
	
	using reverse_iterator			= util::ReverseIterator<iterator>;
	using const_reverse_iterator	= util::ReverseIterator<const_iterator>;

public:
	DynamicArray() noexcept
		: _data() {}

	explicit DynamicArray(size_type count)
		: _data() {
		this->_constructN(count);
	}

	DynamicArray(size_type count, const T& val)
		: _data() {
		this->_constructN(count, val);
	}

	template<class Iter,
		std::enable_if_t<traits::IsIterator<Iter>, int> = 0>
	DynamicArray(Iter first, Iter last)
		: _data() {
		this->_constructN(static_cast<size_type>(std::distance(first, last)), first, last);
	}

	DynamicArray(std::initializer_list<T> initList)
		: _data() {
		this->_constructN(initList.size(), initList.begin(), initList.end());
	}

	~DynamicArray() noexcept {
		this->clear();
	}

	[[nodiscard]] size_type size() const noexcept {
		return static_cast<size_type>(_data.last - _data.first);
	}

	void clear() noexcept {
		// Erase all
		memory::destructRange(_data.first, _data.last);
		memory::deallocate(_data.first, _data.end - _data.first);
		_data.first = nullptr;
		_data.last = nullptr;
		_data.end = nullptr;
	}

	void print() noexcept {
		for (auto i = _data.first; i != _data.last; ++i) {
			std::cout << i << " ";
		}
		std::cout << "Size = " << this->size() << "\n";
	}

private:
	template<class... Args>
	void _constructN(size_type count, Args&&... args) {

		auto newFirst = static_cast<pointer>(memory::allocate(count, sizeof(value_type)));
		auto newLast = newFirst;
		if constexpr (sizeof...(Args) == 0) {
			newLast = memory::uninitializedDefaultConstruct(newFirst, count);
		}
		else if constexpr (sizeof...(Args) == 1) {
			newLast = memory::uninitializedFill(newFirst, count, std::forward<Args>(args)...);
			std::cout << "Fill\n";
		}
		else if constexpr (sizeof...(Args) == 2) {
			newLast = memory::uninitializedCopy(std::forward<Args>(args)..., newFirst);
		}
		else {
			throw std::logic_error("Should be unreachable");
		}

		_data.first = newFirst;
		_data.last = newLast;
		_data.end = newLast;
		return;
	}


private:
	_DynamicArrVal _data;
};

#endif // DYNAMIC_ARRAY_H