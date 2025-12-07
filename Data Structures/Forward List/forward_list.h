#pragma once
#ifndef FORWARD_LIST_H
#define FORWARD_LIST_H

#include"memory.hpp"
#include"type_traits.hpp"

template<class FwdListVal>
class _ForwardListConstIterator {
private:
	using _NodePointer = typename FwdListVal::NodePointer;

public:
	using iterator_category	= std::forward_iterator_tag;
	using value_type		= typename FwdListVal::value_type;
	using difference_type	= typename FwdListVal::difference_type;
	using pointer			= typename FwdListVal::const_pointer;
	using reference			= const value_type&;

	_ForwardListConstIterator() noexcept
		: _ptr() {}

	_ForwardListConstIterator(const _NodePointer ptr) noexcept
		: _ptr(ptr) {}

	[[nodiscard]] _NodePointer getPointer() const noexcept {
		return _ptr;
	}

	[[nodiscard]] reference operator*() const noexcept {
		return _ptr->value;
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_ForwardListConstIterator& operator++() noexcept {
		_ptr = _ptr->next;
		return *this;
	}

	_ForwardListConstIterator& operator++(int) noexcept {
		_ForwardListConstIterator temp = *this;
		_ptr = _ptr->next;
		return temp;
	}

	[[nodiscard]] bool operator==(const _ForwardListConstIterator& rhs) const noexcept {
		return _ptr == rhs.getPointer();
	}

	[[nodiscard]] bool operator!=(const _ForwardListConstIterator& rhs) const noexcept {
		return !(*this == rhs);
	}

private:
	_NodePointer _ptr;
};

template<class FwdListVal>
class _ForwardListIterator : public _ForwardListConstIterator<FwdListVal> {
private:
	using _BaseIter	= _ForwardListConstIterator<FwdListVal>;
	using _BaseIter::_BaseIter; // To inherit _BaseIter's constructors

public:
	using iterator_category	= std::forward_iterator_tag;
	using value_type		= typename FwdListVal::value_type;
	using difference_type	= typename FwdListVal::difference_type;
	using pointer			= typename FwdListVal::pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_ForwardListIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	_ForwardListIterator& operator++(int) noexcept {
		_ForwardListIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}
};

template<class ValueType>
struct _ForwardListNode {
	using NodePointer = _ForwardListNode*;

	_ForwardListNode() = default;

	_ForwardListNode(const _ForwardListNode&)				= delete;
	_ForwardListNode& operator=(const _ForwardListNode&)	= delete;

	static void freeNode(NodePointer node) noexcept {
		memory::destructInPlace(node->next);
		memory::destructInPlace(node->value);
		memory::deallocate(node, 1);
	}

	NodePointer	next; // Important! (next goes before value)
	ValueType	value;
};

template<class ValueType, class SizeType, class DiffType, class Pointer, class ConstPointer, class NodeType>
class _ForwardListValue {
public:
	using Node			= NodeType;
	using NodePointer	= typename Node::NodePointer;

	using value_type		= ValueType;
	using size_type			= SizeType;
	using difference_type	= DiffType;
	using pointer			= Pointer;
	using const_pointer		= ConstPointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	_ForwardListValue() noexcept
		: head(), size(0) {}

	NodePointer beforeHead() const noexcept {
		// Returns pointer to "before begin" pseudo node
		return static_cast<NodePointer>(std::addressof(reinterpret_cast<Node&>(const_cast<NodePointer&>(head))));
	}

	NodePointer head;
	size_type	size; // Unimplemented
};

template<class FwdListVal>
struct _ForwardListInsertOperation {
	using Node			= typename FwdListVal::Node;
	using NodePointer	= typename FwdListVal::NodePointer;

	using size_type		= typename FwdListVal::size_type;

	_ForwardListInsertOperation()
		: _head(), _tail() {}

	_ForwardListInsertOperation(const _ForwardListInsertOperation&)				= delete;
	_ForwardListInsertOperation& operator=(const _ForwardListInsertOperation&)	= delete;

	~_ForwardListInsertOperation() {
		if (_tail == NodePointer{}) {
			return;
		}

		memory::constructInPlace(_tail->next, NodePointer{});
		while (_head) {
			Node::freeNode(std::exchange(_head, _head->next));
		}
	}

	template<class... Args>
	void appendN(size_type count, const Args&... args) {
		// Append count elements by constructing in place using args
		if (count <= 0) {
			return;
		}

		NodePointer newNode{};
		if (_tail == NodePointer{}) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(Node)));
			memory::constructInPlace(newNode->value, args...);
			_head = newNode;
			_tail = newNode;
			--count;
		}

		for (; 0 < count; --count) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(Node)));
			memory::constructInPlace(newNode->value, args...);
			memory::constructInPlace(_tail->next, newNode);
			_tail = newNode;
		}
	}

	template<class Iter>
	void appendRange(Iter first, const Iter last) {
		// Append range [first, last)
		if (first == last) {
			return;
		}

		NodePointer newNode{};
		if (_tail == NodePointer{}) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(Node)));
			memory::constructInPlace(newNode->value, *first);
			_head = newNode;
			_tail = newNode;
			++first;
		}

		for (; first != last; ++first) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(Node)));
			memory::constructInPlace(newNode->value, *first);
			memory::constructInPlace(_tail->next, newNode);
			_tail = newNode;
		}
	}

	NodePointer attachAfter(NodePointer node) {
		// Attach elements in *this after node, reset *this to default-initialized state
		const auto oldTail = _tail;
		if (oldTail == NodePointer{}) {
			return node;
		}

		memory::constructInPlace(oldTail->next, node->next);
		node->next	= _head;
		_tail		= NodePointer{};

		return oldTail;
	}

private:
	NodePointer _head; // Points to the first constructed node
	NodePointer _tail; // Points to the most recently constructed node
};

template<class T>
class ForwardList {
public:
	using value_type		= T;
	using size_type			= std::size_t;
	using difference_type	= std::ptrdiff_t;
	using pointer			= T*;
	using const_pointer		= const T*;
	using reference			= T&;
	using const_reference	= const T&;

private:
	using _Node			= _ForwardListNode<T>;
	using _NodePointer	= _Node::NodePointer;

	using _FwdListVal	= _ForwardListValue<value_type, size_type, difference_type, pointer, const_pointer, _Node>;

public:
	using iterator			= _ForwardListIterator<_FwdListVal>;
	using const_iterator	= _ForwardListConstIterator<_FwdListVal>;

public:
	ForwardList()
		: _data() {} // Construct empty list

	explicit ForwardList(const size_type count)
		: _data() {
		// Construct count * T()
		_ForwardListInsertOperation<_FwdListVal> insertOp;
		insertOp.appendN(count);
		insertOp.attachAfter(_data.beforeHead());
	}

	ForwardList(const size_type count, const T& val)
		: _data() {
		// Construct count * val
		_ForwardListInsertOperation<_FwdListVal> insertOp;
		insertOp.appendN(count, val);
		insertOp.attachAfter(_data.beforeHead());
	}

	template<class Iter,
		std::enable_if_t<traits::IsIterator<Iter>, bool> = true>
	ForwardList(Iter first, const Iter last)
		: _data() {
		// Construct from range [first, last)
		_ForwardListInsertOperation<_FwdListVal> insertOp;
		insertOp.appendRange(first, last);
		insertOp.attachAfter(_data.beforeHead());
	}

	ForwardList(const ForwardList& other)
		: _data() {
		// Copy from other
		_ForwardListInsertOperation<_FwdListVal> insertOp;
		insertOp.appendRange(other.begin(), other.end());
		insertOp.attachAfter(_data.beforeHead());
	}

	ForwardList& operator=(const ForwardList& rhs) {
		if (this == std::addressof(rhs)) {
			return *this;
		}

		this->assign(rhs.begin(), rhs.end());
		return *this;
	}

	ForwardList(ForwardList&& other) noexcept
		: _data() {
		// Take other's contents
		_data.head = std::exchange(other._data.head, nullptr);
	}

	ForwardList& operator=(ForwardList&& rhs) noexcept {
		if (this == std::addressof(rhs)) {
			return *this;
		}

		this->clear();
		_data.head = std::exchange(rhs._data.head, nullptr);
		return *this;
	}

	ForwardList(std::initializer_list<T> initList)
		: _data() {
		// Construct from initializer list
		this->insertAfter(this->beforeBegin(), initList.begin(), initList.end());
	}

	ForwardList& operator=(std::initializer_list<T> initList) {
		this->assign(initList.begin(), initList.end());
		return *this;
	}

	~ForwardList() noexcept {
		this->clear();
	}

	[[nodiscard]] iterator beforeBegin() noexcept {
		return iterator(_data.beforeHead());
	}

	[[nodiscard]] const_iterator beforeBegin() const noexcept {
		return const_iterator(_data.beforeHead());
	}

	[[nodiscard]] iterator begin() noexcept {
		return iterator(_data.head);
	}

	[[nodiscard]] const_iterator begin() const noexcept {
		return const_iterator(_data.head);
	}

	[[nodiscard]] iterator end() noexcept {
		return iterator(nullptr);
	}

	[[nodiscard]] const_iterator end() const noexcept {
		return const_iterator(nullptr);
	}

	[[nodiscard]] const_iterator cbeforeBegin() const noexcept {
		return this->beforeBegin();
	}

	[[nodiscard]] const_iterator cbegin() const noexcept {
		return this->begin();
	}

	[[nodiscard]] const_iterator cend() const noexcept {
		return this->end();
	}

	[[nodiscard]] reference front() noexcept {
		return _data.head->value; // UB
	}

	[[nodiscard]] const_reference front() const noexcept {
		return _data.head->value; // UB
	}

	[[nodiscard]] size_type size() const noexcept {
		return _data.size;
	}

	[[nodiscard]] size_type maxSize() const noexcept {
		return static_cast<size_type>(-1) / sizeof(_Node);
	}

	[[nodiscard]] bool isEmpty() const noexcept {
		return _data.head == nullptr;
	}

	void assign(const size_type count, const T& val) {
		// Assign count * val
		this->clear();
		this->insertAfter(this->beforeBegin(), count, val);
	}

	template<class Iter,
		std::enable_if_t<traits::IsIterator<Iter>, bool> = true>
	void assign(Iter first, const Iter last) {
		// Assign range [first, last)
		_NodePointer currNode = _data.beforeHead();
		for (; first != last; ++first) {
			const _NodePointer nextNode = currNode->next;
			if (!nextNode) {
				// Runs out of nodes, insert the remaining nodes to *this
				_ForwardListInsertOperation<_FwdListVal> insertOp;
				insertOp.appendRange(first, last);
				insertOp.attachAfter(currNode);
				return;
			}
			// Assign [first, last) to current nodes
			nextNode->value = *first;
			currNode = nextNode;
		}
		// Trim excessive nodes from *this
		for (_NodePointer nextNode = std::exchange(currNode->next, nullptr); nextNode;) {
			_Node::freeNode(std::exchange(nextNode, nextNode->next));
		}
	}

	void pushFront(const T& val) {
		// Insert at begin by copying val
		this->_insertAfter(_data.beforeHead(), val);
	}

	void pushFront(T&& val) {
		// Insert at begin by moving val
		this->_insertAfter(_data.beforeHead(), std::move(val));
	}

	template<class... Args>
	decltype(auto) emplaceFront(Args&&... args) {
		// Insert at begin by constructing in place using args
		this->_insertAfter(_data.beforeHead(), std::forward<Args>(args)...);
		return this->front(); // UB
	}

	template<class... Args>
	iterator emplaceAfter(const_iterator pos, Args&&... args) {
		// Insert after pos by constructing in place using args
		this->_insertAfter(pos.getPointer(), std::forward<Args>(args)...);
		return iterator(pos.getPointer()->next);
	}

	iterator insertAfter(const_iterator pos, const T& val) {
		// Insert after pos by copying val
		this->_insertAfter(pos.getPointer(), val);
		return iterator(pos.getPointer()->next);
	}

	iterator insertAfter(const_iterator pos, T&& val) {
		// Insert after pos by copying val
		return this->emplaceAfter(pos, std::move(val));
	}

	iterator insertAfter(const_iterator pos, const size_type count, const T& val) {
		// Insert count * val after pos
		if (count != 0) {
			_ForwardListInsertOperation<_FwdListVal> insertOp;
			insertOp.appendN(count, val);
			return iterator(insertOp.attachAfter(pos.getPointer()));
		}
		return static_cast<iterator>(pos);
	}

	template<class InputIter,
		std::enable_if_t<traits::IsInputIter<InputIter>, bool> = true>
	iterator insertAfter(const_iterator pos, const InputIter first, const InputIter last) {
		// Insert range [first, last) after pos
		_NodePointer node = pos.getPointer();
		if (first != last) {
			_ForwardListInsertOperation<_FwdListVal> insertOp;
			insertOp.appendRange(first, last);
			return iterator(insertOp.attachAfter(pos.getPointer()));
		}
		return static_cast<iterator>(pos);
	}

	iterator insertAfter(const_iterator pos, std::initializer_list<T> initList) {
		// Insert initList after pos
		return this->insertAfter(pos, initList.begin(), initList.end());
	}

	void popFront() noexcept {
		// Erase at begin
		this->_eraseAfter(_data.beforeHead()); // UB
	}

	iterator eraseAfter(const_iterator pos) noexcept {
		// Erase after pos
		this->_eraseAfter(pos.getPointer()); // UB
		return iterator(pos.getPointer()->next);
	}

	iterator eraseAfter(const_iterator first, const_iterator last) noexcept {
		// Erase range (first, last)
		_NodePointer currNode = first.getPointer();
		_NodePointer lastNode = last.getPointer();
		if (currNode != lastNode) {
			for (_NodePointer nextNode = currNode->next; nextNode != lastNode;) {
				currNode->next = nextNode->next;
				_Node::freeNode(std::exchange(nextNode, currNode->next));
			}
		}
		return iterator(lastNode);
	}

	void clear() noexcept {
		// Erase all
		for (_NodePointer currNode = std::exchange(_data.head, nullptr); currNode;) {
			_Node::freeNode(std::exchange(currNode, currNode->next));
		}
	}

	void swap(ForwardList& other) noexcept {
		// Swap contents with other
		using std::swap;
		if (this != std::addressof(other)) {
			swap(_data.head, other._data.head); // ADL
			std::swap(_data.size, other._data.size);
		}
	}

	void spliceAfter(const_iterator pos, ForwardList<T>& other) noexcept {
		// Splice all of other after pos
		if (this != std::addressof(other) && !other.isEmpty()) {
			const auto first	= other.beforeBegin();
			const auto last		= other.end();
			this->_spliceAfter(pos.getPointer(), first.getPointer(), last.getPointer()); // UB
		}
	}

	void spliceAfter(const_iterator pos, const_iterator before) noexcept {
		// Splice range (before, before + 2) after pos
		return this->_spliceAfter(pos.getPointer(), before.getPointer());
	}

	void spliceAfter(const_iterator pos, const_iterator first, const_iterator last) noexcept {
		// Splice range (first, last) after pos
		return this->_spliceAfter(pos.getPointer(), first.getPointer(), last.getPointer()); // UB
	}

	[[nodiscard]] const_iterator find(const T& key) const noexcept {
		// Find the first occurence of key
		_NodePointer currNode = _data.head;
		while (currNode && currNode->value != key) {
			currNode = currNode->next;
		}
		return const_iterator(currNode);
	}

	[[nodiscard]] auto compare(const ForwardList& other) const noexcept {
		// Compare with other by each element
		_NodePointer firstNode = _data.head, secondNode = other._data.head;
		for (; firstNode && secondNode; firstNode = firstNode->next, secondNode = secondNode->next) {
			if (firstNode->value == secondNode->value) {
				continue;
			}
#if _MSVC_LANG >= 202002L
			return (firstNode->value < secondNode->value) ? std::strong_ordering::less : std::strong_ordering::greater;
#else
			return (firstNode->value < secondNode->value) ? -1 : 1;
#endif // Has C++20
		}

#if _MSVC_LANG >= 202002L
		if (!(firstNode && secondNode)) {
			return std::strong_ordering::equal;
		}
		return (secondNode) ? std::strong_ordering::less : std::strong_ordering::greater;
#else
		if (!(firstNode && secondNode)) {
			return 0;
		}
		return (secondNode) ? -1 : 1;
#endif // Has C++20
	}

	[[nodiscard]] size_type count() const noexcept { // Deprecated
		// Count all elements in O(n) time
		size_type count = 0;
		for (_NodePointer currNode = _data.head; currNode; currNode = currNode->next) {
			++count;
		}
		return count;
	}

	[[nodiscard]] size_type count(const T& key) const noexcept {
		// Count occurences of key
		return this->countIf([&](const T& val) -> bool { return val == key; }); // UB
	}

	template<class UnaryPred>
	[[nodiscard]] size_type countIf(UnaryPred pred) const noexcept {
		// Count elements satisfying pred
		size_type count = 0;
		for (_NodePointer currNode = _data.head; currNode; currNode = currNode->next) {
			if (pred(currNode->value)) { // UB
				++count;
			}
		}
		return count;
	}

	size_type remove(const T& key) noexcept {
		// Remove occurences of key
		return this->removeAfter(key, this->beforeBegin(), this->end()); // UB
	}

	size_type removeAfter(const T& key, const_iterator first, const_iterator last) noexcept {
		// Remove occurences of key in range (first, last)
		return this->_removeIfAfter([&](const T& val) -> bool { return val == key; }, first.getPointer(), last.getPointer()); // UB
	}

	template<class UnaryPred>
	size_type removeIf(UnaryPred pred) noexcept {
		// Remove elements satisfying pred
		return this->removeIfAfter(pred, this->beforeBegin(), this->end()); // UB
	}

	template<class UnaryPred>
	size_type removeIfAfter(UnaryPred pred, const_iterator first, const_iterator last) noexcept {
		// Remove elements satisfying pred in range (first, last)
		return this->_removeIfAfter(pred, first.getPointer(), last.getPointer()); // UB
	}

	void reverse() noexcept {
		// Reverse elements order
		if (!_data.head) {
			return;
		}

		for (_NodePointer prevNode{}, currNode = _data.head, nextNode = currNode->next;;) {
			currNode->next = prevNode;
			if (!nextNode) {
				_data.head = currNode;
				return;
			}

			prevNode = currNode;
			currNode = nextNode;
			nextNode = nextNode->next;
		}
	}

	size_type uniqueGroup() noexcept {
		// Remove consecutive duplicates
		return this->uniqueGroup([&](const T& lhs, const T& rhs) -> bool { return lhs == rhs; }); // UB
	}

	template<class BinaryPred>
	size_type uniqueGroup(BinaryPred pred) noexcept {
		// Remove consecutive elements satisfying pred
		size_type removed = 0;
		if (_data.head) {
			for (_NodePointer currNode = _data.head, nextNode = currNode->next; nextNode;) {
				if (pred(currNode->value, nextNode->value)) { // UB
					_Node::freeNode(std::exchange(currNode->next, nextNode->next));
					++removed;
				} else {
					currNode = nextNode;
				}
				nextNode = currNode->next;
			}
		}
		return removed;
	}
	
	template<class Compare = std::less<>>
	void merge(ForwardList<T>& other, Compare comp = Compare{}) noexcept {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		if (this == std::addressof(other) || other.isEmpty()) {
			return;
		}

		if (this->isEmpty()) {
			_data.head = std::exchange(other._data.head, nullptr);
			return;
		}

		for (_NodePointer beforeFirst = _data.beforeHead(), beforeMid = other._data.beforeHead(), midNode = other._data.head;;) {
			// Find position in the first range where insertion is needed
			_NodePointer firstNode{};
			while (true) {
				firstNode = beforeFirst->next;
				if (!firstNode) { // First range is exhausted, return
					beforeFirst->next = midNode;
					other._data.head = nullptr;
					return;
				}

				if (comp(midNode->value, firstNode->value)) { // UB
					break;
				}
				beforeFirst = firstNode;
			}
			// Find sub-range in the second range to insert into the first range
			_NodePointer currNode = midNode, nextNode{};
			while (true) {
				nextNode = currNode->next;
				if (!nextNode) { // Second range is exhausted
					break;
				}

				if (!comp(nextNode->value, firstNode->value)) { // UB
					break;
				}
				currNode = nextNode;
			}
			// Insert [midNode, currNode] between beforeFirst and firstNode
			beforeFirst->next	= midNode;
			beforeMid->next		= nextNode;
			currNode->next		= firstNode;
			if (!nextNode) { // Second range is exhausted, return
				return;
			}
			// Advance node pointers for both ranges
			beforeFirst = firstNode;
			midNode = nextNode;
		}
	}

	template<class Compare = std::less<>>
	void sort(Compare comp = Compare{}) noexcept {
		// Sort whole list using merge sort, elements are compared using comp
		this->_sort(_data.beforeHead(), comp); // UB
	}

private:
	template<class... Args>
	void _insertAfter(_NodePointer node, Args&&... args) {
		// Insert after node by perfect forwarding args
		const _NodePointer newNode = static_cast<_NodePointer>(memory::allocate(1, sizeof(_Node)));
		memory::constructInPlace(newNode->value, std::forward<Args>(args)...);
		memory::constructInPlace(newNode->next, node->next);
		node->next = newNode;
	}

	void _eraseAfter(_NodePointer node) noexcept {
		// Erase after node
		_Node::freeNode(std::exchange(node->next, node->next->next)); // UB
	}

	void _spliceAfter(_NodePointer node, _NodePointer prevNode) noexcept {
		// Splice range (prevNode, prevNode + 2) after node
		if (node != prevNode) {
			const _NodePointer currNode = prevNode->next;
			if (node != currNode) {
				prevNode->next	= currNode->next;
				currNode->next	= node->next;
				node->next		= currNode;
			}
		}
	}

	void _spliceAfter(_NodePointer node, _NodePointer first, _NodePointer last) noexcept {
		// Splice range (first, last) after node
		if (first == last || first->next == last) {
			return;
		}
		// Find prev of last
		_NodePointer currNode = first->next;
		while (currNode->next != last) {
			currNode = currNode->next;
		}
		// UB: if node is in (first, last), this will lead to 2 unowned, circular node chains
		const _NodePointer nextNode	= first->next;
		first->next					= last;
		currNode->next				= node->next;
		node->next					= nextNode;
	}

	template<class UnaryPred>
	size_type _removeIfAfter(UnaryPred pred, _NodePointer first, _NodePointer last) noexcept {
		// Remove elements satisfying pred in range (first, last)
		size_type removed = 0;
		for (_NodePointer currNode = first->next; currNode != last;) {
			if (pred(currNode->value)) { // UB
				_Node::freeNode(std::exchange(first->next, currNode->next));
				++removed;
			} else {
				first = currNode;
			}
			currNode = first->next;
		}
		return removed;
	}

	template<class Compare>
	_NodePointer _merge(_NodePointer beforeFirst, _NodePointer beforeMid, _NodePointer beforeLast, Compare comp) noexcept {
		// Merge sorted range (beforeFirst, beforeMid] and (beforeMid, beforeLast]
		for (_NodePointer midNode = beforeMid->next;;) {
			// Find position in the first range where insertion is needed
			_NodePointer firstNode{};
			while (true) {
				firstNode = beforeFirst->next;
				if (beforeFirst == beforeMid) { // First range is exhausted, return beforeLast
					return beforeLast;
				}

				if (comp(midNode->value, firstNode->value)) { // UB
					break;
				}
				beforeFirst = firstNode;
			}
			// Find sub-range in the second range to insert into the first range
			_NodePointer currNode = midNode, nextNode{};
			while (true) {
				nextNode = currNode->next;
				if (currNode == beforeLast) { // Second range is exhausted
					break;
				}

				if (!comp(nextNode->value, firstNode->value)) { // UB
					break;
				}
				currNode = nextNode;
			}
			// Insert [midNode, currNode] between beforeFirst and firstNode
			beforeFirst->next = midNode;
			beforeMid->next = nextNode;
			currNode->next = firstNode;
			if (currNode == beforeLast) { // Second range is exhausted, return beforeMid
				return beforeMid;
			}
			// Advance node pointers for both ranges
			beforeFirst = firstNode;
			midNode = nextNode;
		}
	}

	template<class Compare>
	_NodePointer _sort(_NodePointer beforeFirst, size_type length, Compare comp) noexcept {
		// Sort range (beforeFirst, beforeFirst + length), or until nullptr is encountered
		if (length <= 2) {
			// Sort 2 elements
			const _NodePointer firstNode = beforeFirst->next;
			if (!firstNode) {
				return beforeFirst;
			}

			const _NodePointer lastNode = firstNode->next;
			if (!lastNode || comp(firstNode->value, lastNode->value)) { // UB
				return firstNode;
			}
			// Swap firstNode and lastNode
			firstNode->next = std::exchange(lastNode->next, firstNode);
			beforeFirst->next = lastNode;
			return lastNode;
		}
		// Sort top-down half length
		const size_type halfLength = length / 2;
		const _NodePointer beforeMid = this->_sort(beforeFirst, halfLength, comp);
		if (!beforeMid->next) {
			return beforeMid;
		}

		const _NodePointer beforeLast = this->_sort(beforeMid, halfLength, comp);
		return this->_merge(beforeFirst, beforeMid, beforeLast, comp); // UB
	}

	template<class Compare>
	void _sort(_NodePointer beforeFirst, Compare comp) noexcept {
		// Sort whole list bottom-up
		_NodePointer beforeMid = this->_sort(beforeFirst, 2, comp);
		for (size_type length = 2;; length *= 2) {
			if (!beforeMid->next) {
				return;
			}

			const _NodePointer beforeLast = this->_sort(beforeMid, length, comp);
			beforeMid = this->_merge(beforeFirst, beforeMid, beforeLast, comp); // UB
		}
	}
private:
	_FwdListVal _data;
};

#if _MSVC_LANG >= 202002L
template<class T>
[[nodiscard]] std::strong_ordering operator<=>(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return lhs.compare(rhs);
}
#else
template<class T>
[[nodiscard]] bool operator==(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return lhs.compare(rhs) == 0;
}

template<class T>
[[nodiscard]] bool operator!=(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return !(lhs == rhs);
}

template<class T>
[[nodiscard]] bool operator<(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return lhs.compare(rhs) < 0;
}

template<class T>
[[nodiscard]] bool operator>(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return rhs < lhs;
}

template<class T>
[[nodiscard]] bool operator<=(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return !(rhs < lhs);
}

template<class T>
[[nodiscard]] bool operator>=(const ForwardList<T>& lhs, const ForwardList<T>& rhs) noexcept {
	return !(lhs < rhs);
}
#endif // Has C++20
#endif // FORWARD_LIST_HPP