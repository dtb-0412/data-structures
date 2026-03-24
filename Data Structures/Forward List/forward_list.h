#pragma once
#ifndef FORWARD_LIST_H
#define FORWARD_LIST_H

#include<stdexcept>

#include"memory.hpp"
#include"type_traits.hpp"

template<class FwdListVal>
class ForwardListConstIterator {
private:
	using _NodePointer = typename FwdListVal::NodePointer;

public:
	using iterator_category	= std::forward_iterator_tag;
	using value_type		= typename FwdListVal::ValueType;
	using difference_type	= typename FwdListVal::DifferenceType;
	using pointer			= typename FwdListVal::ConstPointer;
	using reference			= const value_type&;

	ForwardListConstIterator() noexcept
		: ptr() {}

	ForwardListConstIterator(const _NodePointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] reference operator*() const noexcept {
		return ptr->value;
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	ForwardListConstIterator& operator++() noexcept {
		ptr = ptr->next;
		return *this;
	}

	ForwardListConstIterator operator++(int) noexcept {
		ForwardListConstIterator temp = *this;
		ptr = ptr->next;
		return temp;
	}

	[[nodiscard]] bool operator==(const ForwardListConstIterator& rhs) const noexcept {
		return ptr == rhs.ptr;
	}

	[[nodiscard]] bool operator!=(const ForwardListConstIterator& rhs) const noexcept {
		return !(*this == rhs);
	}

public:
	_NodePointer ptr;
};

template<class FwdListVal>
class ForwardListIterator : public ForwardListConstIterator<FwdListVal> {
private:
	using _BaseIter	= ForwardListConstIterator<FwdListVal>;
	using _BaseIter::_BaseIter;

public:
	using iterator_category	= std::forward_iterator_tag;
	using value_type		= typename FwdListVal::ValueType;
	using difference_type	= typename FwdListVal::DifferenceType;
	using pointer			= typename FwdListVal::Pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	ForwardListIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	ForwardListIterator operator++(int) noexcept {
		ForwardListIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}
};

template<class ValueT>
struct ForwardListNode {
	using NodePointer = ForwardListNode*;

	ForwardListNode() = default;

	ForwardListNode(const ForwardListNode&)				= delete;
	ForwardListNode& operator=(const ForwardListNode&)	= delete;

	static void free_node(NodePointer node) noexcept {
		memory::destruct_in_place(node->next);
		memory::destruct_in_place(node->value);
		memory::deallocate(node, sizeof(ForwardListNode));
	}

	NodePointer	next; // Member next MUST come first.
	ValueT		value;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class ForwardListValue {
public:
	using NodeType			= NodeT;
	using NodePointer		= typename NodeType::NodePointer;

	using ValueType			= ValueT;
	using SizeType			= SizeT;
	using DifferenceType	= DiffT;
	using Pointer			= Ptr;
	using ConstPointer		= ConstPtr;
	using Reference			= ValueType&;
	using ConstReference	= const ValueType&;

	ForwardListValue() noexcept
		: head() {}

	[[nodiscard]] NodePointer before_head() const noexcept {
		// Returns pointer to "before head" pseudo node
		/*
		First, cast away head's constness, then reinterpret cast it from NodePointer to NodeType&.
		After the reinterpret cast, head's memory location overlaps with member next of the "before head" pseudo node.
		The result is the pseudo node which points to head through its member next.
		Take the pseudo node's address, cast it to a NodePointer and we get a "before head" NodePointer.

		Important:
		The "before head" NodePointer can only be used as a sentinel for insertions to the front of the list.
		Only accessing the member next is valid, all other operations on the pointer (dereferencing, accessing its value, ...) are UB.
		*/
		return static_cast<NodePointer>(std::addressof( // Step 3: Take the address of the pseudo node and cast it to a NodePointer.
			reinterpret_cast<NodeType&>(				// Step 2: Reinterpret cast head from a NodePointer to a NodeType&.
			const_cast<NodePointer&>(head)				// Step 1: Const cast head to allow modification.
		)));
	}

	void clear() noexcept {
		// Clear all nodes
		for (NodePointer currNode = head; currNode;) {
			NodeType::free_node(std::exchange(currNode, currNode->next));
		}
	}

	void swap(ForwardListValue& other) noexcept {
		// Swap contents with other
		using std::swap;
		swap(head, other.head);
	}

	NodePointer head;
};

template<class FwdListVal>
struct ForwardListInsertOperation {
	using NodeType		= typename FwdListVal::NodeType;
	using NodePointer	= typename FwdListVal::NodePointer;

	using SizeType		= typename FwdListVal::SizeType;

	ForwardListInsertOperation()
		: _head(), _tail() {}

	ForwardListInsertOperation(const ForwardListInsertOperation&)				= delete;
	ForwardListInsertOperation& operator=(const ForwardListInsertOperation&)	= delete;

	~ForwardListInsertOperation() {
		if (_tail == NodePointer{}) {
			return;
		}

		memory::construct_in_place(_tail->next, NodePointer{});
		while (_head) {
			NodeType::free_node(std::exchange(_head, _head->next));
		}
	}

	template<class... Args>
	void append_n(SizeType count, const Args&... args) {
		// Append count elements by constructing in place using args
		if (count <= 0) {
			return;
		}

		NodePointer newNode{};
		if (_tail == NodePointer{}) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(NodeType)));
			memory::construct_in_place(newNode->value, args...);
			_head = newNode;
			_tail = newNode;
			--count;
		}

		for (; 0 < count; --count) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(NodeType)));
			memory::construct_in_place(newNode->value, args...);
			memory::construct_in_place(_tail->next, newNode);
			_tail = newNode;
		}
	}

	template<class InputIter>
	void append_range(InputIter first, const InputIter last) {
		// Append range [first, last)
		if (first == last) {
			return;
		}

		NodePointer newNode{};
		if (_tail == NodePointer{}) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(NodeType)));
			memory::construct_in_place(newNode->value, *first);
			_head = newNode;
			_tail = newNode;
			++first;
		}

		for (; first != last; ++first) {
			newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(NodeType)));
			memory::construct_in_place(newNode->value, *first);
			memory::construct_in_place(_tail->next, newNode);
			_tail = newNode;
		}
	}

	NodePointer attach_after(NodePointer node) {
		// Attach elements in *this after node, reset *this to default-initialized state
		const auto oldTail = _tail;
		if (oldTail == NodePointer{}) {
			return node;
		}

		memory::construct_in_place(oldTail->next, node->next);
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
	using _NodeType		= ForwardListNode<T>;
	using _NodePointer	= typename _NodeType::NodePointer;

	using _FwdListValue	= ForwardListValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

public:
	using iterator			= ForwardListIterator<_FwdListValue>;
	using const_iterator	= ForwardListConstIterator<_FwdListValue>;

public:
	ForwardList()
		: _data() {} // Construct empty forward list

	explicit ForwardList(const size_type count)
		: _data() {
		// Construct count * T()
		this->_construct(count);
	}

	ForwardList(const size_type count, const T& val)
		: _data() {
		// Construct count * val
		this->_construct(count, val);
	}

	template<class InputIter,
		std::enable_if_t<traits::is_input_iter<InputIter>, bool> = true>
	ForwardList(InputIter first, const InputIter last)
		: _data() {
		// Construct from range [first, last)
		this->_construct(static_cast<size_type>(std::distance(first, last)), first, last);
	}

	ForwardList(const ForwardList& other)
		: _data() {
		// Copy from other
		this->_construct(other.size(), other.begin(), other.end());
	}

	ForwardList& operator=(const ForwardList& other) {
		if (this != std::addressof(other)) {
			this->assign(other.begin(), other.end());
		}
		return *this;
	}

	ForwardList(ForwardList&& other) noexcept
		: _data() {
		_data.swap(other._data);
	}

	ForwardList& operator=(ForwardList&& other) noexcept {
		if (this != std::addressof(other)) {
			this->clear();
			_data.swap(other._data);
		}
		return *this;
	}

	ForwardList(std::initializer_list<T> initList)
		: _data() {
		// Construct from initializer list
		this->_construct(initList.size(), initList.begin(), initList.end());
	}

	ForwardList& operator=(std::initializer_list<T> initList) {
		this->assign(initList.begin(), initList.end());
		return *this;
	}

	~ForwardList() noexcept {
		_data.clear();
	}

	[[nodiscard]] iterator before_begin() noexcept {
		return iterator(_data.before_head());
	}

	[[nodiscard]] const_iterator before_begin() const noexcept {
		return const_iterator(_data.before_head());
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

	[[nodiscard]] const_iterator cbefore_begin() const noexcept {
		return this->before_begin();
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
		return this->count();
	}

	[[nodiscard]] size_type max_size() const noexcept {
		return static_cast<size_type>(-1) / sizeof(_NodeType);
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.head == nullptr;
	}

	void assign(const size_type count, const T& val) {
		// Assign count * val
		this->clear();
		this->insert_after(this->before_begin(), count, val);
	}

	template<class InputIter,
		std::enable_if_t<traits::is_input_iter<InputIter>, bool> = true>
	void assign(InputIter first, const InputIter last) {
		// Assign range [first, last)
		_NodePointer currNode = _data.before_head();
		for (; first != last; ++first) {
			const _NodePointer nextNode = currNode->next;
			if (!nextNode) {
				// Runs out of nodes, insert the remaining nodes to *this
				ForwardListInsertOperation<_FwdListValue> insertOp;
				insertOp.append_range(first, last);
				insertOp.attach_after(currNode);
				return;
			}
			// Assign [first, last) to current nodes
			nextNode->value = *first;
			currNode = nextNode;
		}
		// Trim excessive nodes from *this
		for (_NodePointer nextNode = std::exchange(currNode->next, nullptr); nextNode;) {
			_NodeType::free_node(std::exchange(nextNode, nextNode->next));
		}
	}

	void push_front(const T& val) {
		// Insert at begin by copying val
		this->_insert_after(_data.before_head(), val);
	}

	void push_front(T&& val) {
		// Insert at begin by moving val
		this->_insert_after(_data.before_head(), std::move(val));
	}

	template<class... Args>
	decltype(auto) emplace_front(Args&&... args) {
		// Insert at begin by constructing in place using args
		this->_insert_after(_data.before_head(), std::forward<Args>(args)...);
		return this->front();
	}

	template<class... Args>
	iterator emplace_after(const_iterator pos, Args&&... args) {
		// Insert after pos by constructing in place using args
		this->_insert_after(pos.ptr, std::forward<Args>(args)...);
		return iterator(pos.ptr->next);
	}

	iterator insert_after(const_iterator pos, const T& val) {
		// Insert after pos by copying val
		this->_insert_after(pos.ptr, val);
		return iterator(pos.ptr->next);
	}

	iterator insert_after(const_iterator pos, T&& val) {
		// Insert after pos by copying val
		return this->emplace_after(pos, std::move(val));
	}

	iterator insert_after(const_iterator pos, const size_type count, const T& val) {
		// Insert count * val after pos
		if (count != 0) {
			ForwardListInsertOperation<_FwdListValue> insertOp;
			insertOp.append_n(count, val);
			return iterator(insertOp.attach_after(pos.ptr));
		}
		return iterator(pos.ptr);
	}

	template<class InputIter,
		std::enable_if_t<traits::is_input_iter<InputIter>, bool> = true>
	iterator insert_after(const_iterator pos, const InputIter first, const InputIter last) {
		// Insert range [first, last) after pos
		if (first != last) {
			ForwardListInsertOperation<_FwdListValue> insertOp;
			insertOp.append_range(first, last);
			return iterator(insertOp.attach_after(pos.ptr));
		}
		return iterator(pos.ptr);
	}

	iterator insert_after(const_iterator pos, std::initializer_list<T> initList) {
		// Insert initList after pos
		return this->insert_after(pos, initList.begin(), initList.end());
	}

	void pop_front() noexcept {
		// Erase at begin
		this->_erase_after(_data.before_head());
	}

	iterator erase_after(const_iterator pos) noexcept {
		// Erase after pos
		this->_erase_after(pos.ptr);
		return iterator(pos.ptr->next);
	}

	iterator erase_after(const_iterator first, const_iterator last) noexcept {
		// Erase range (first, last)
		_NodePointer currNode = first.ptr;
		_NodePointer lastNode = last.ptr;
		if (currNode != lastNode) {
			for (_NodePointer nextNode = currNode->next; nextNode != lastNode;) {
				currNode->next = nextNode->next;
				_NodeType::free_node(std::exchange(nextNode, currNode->next));
			}
		}
		return iterator(lastNode);
	}

	void clear() noexcept {
		// Erase all
		_data.clear();
		_data.head = nullptr;
	}

	void swap(ForwardList& other) noexcept {
		// Swap contents with other
		if (this != std::addressof(other)) {
			_data.swap(other._data);
		}
	}

	void splice_after(const_iterator pos, ForwardList<T>& other) noexcept {
		// Splice all of other after pos
		if (this != std::addressof(other) && !other.is_empty()) {
			const auto first	= other.before_begin();
			const auto last		= other.end();
			this->_splice_after(pos.ptr, first.ptr, last.ptr);
		}
	}

	void splice_after(const_iterator pos, const_iterator before) noexcept {
		// Splice range (before, before + 2) after pos
		return this->_splice_after(pos.ptr, before.ptr);
	}

	void splice_after(const_iterator pos, const_iterator first, const_iterator last) noexcept {
		// Splice range (first, last) after pos
		return this->_splice_after(pos.ptr, first.ptr, last.ptr);
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
		_NodePointer firstNode	= _data.head;
		_NodePointer secondNode = other._data.head;
		while (firstNode && secondNode) {
			if (firstNode->value == secondNode->value) {
				firstNode	= firstNode->next;
				secondNode	= secondNode->next;
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

	[[nodiscard]] size_type count() const noexcept {
		// Count all elements in O(n) time
		size_type count = 0;
		for (_NodePointer currNode = _data.head; currNode; currNode = currNode->next) {
			++count;
		}
		return count;
	}

	[[nodiscard]] size_type count(const T& key) const noexcept {
		// Count occurences of key
		return this->count_if([&](const T& val) -> bool { return val == key; });
	}

	template<class UnaryPred>
	[[nodiscard]] size_type count_if(UnaryPred pred) const noexcept {
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
		return this->remove_after(key, this->before_begin(), this->end());
	}

	size_type remove_after(const T& key, const_iterator first, const_iterator last) noexcept {
		// Remove occurences of key in range (first, last)
		return this->_remove_if_after([&](const T& val) -> bool { return val == key; }, first.ptr, last.ptr);
	}

	template<class UnaryPred>
	size_type remove_if(UnaryPred pred) noexcept {
		// Remove elements satisfying pred
		return this->remove_if_after(pred, this->before_begin(), this->end());
	}

	template<class UnaryPred>
	size_type remove_if_after(UnaryPred pred, const_iterator first, const_iterator last) noexcept {
		// Remove elements satisfying pred in range (first, last)
		return this->_remove_if_after(pred, first.ptr, last.ptr);
	}

	void reverse() noexcept {
		// Reverse elements order
		if (!_data.head) {
			return;
		}

		_NodePointer prevNode{};
		_NodePointer currNode = _data.head;
		_NodePointer nextNode = currNode->next;
		for (;;) {
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

	size_type unique_group() noexcept {
		// Remove consecutive duplicates
		return this->unique_group([&](const T& lhs, const T& rhs) -> bool { return lhs == rhs; });
	}

	template<class BinaryPred>
	size_type unique_group(BinaryPred pred) noexcept {
		// Remove consecutive elements satisfying pred
		size_type removed = 0;
		if (_data.head) {
			for (_NodePointer currNode = _data.head, nextNode = currNode->next; nextNode;) {
				if (pred(currNode->value, nextNode->value)) { // UB
					_NodeType::free_node(std::exchange(currNode->next, nextNode->next));
					++removed;
				}
				else {
					currNode = nextNode;
				}
				nextNode = currNode->next;
			}
		}
		return removed;
	}
	
	template<class Comp = std::less<>>
	void merge(ForwardList<T>& other, Comp comp = Comp{}) noexcept {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		if (this == std::addressof(other) || other.is_empty()) {
			return;
		}

		if (this->is_empty()) {
			_data.head = std::exchange(other._data.head, nullptr);
			return;
		}

		_NodePointer beforeFirst	= _data.before_head();
		_NodePointer beforeMid		= other._data.before_head();
		_NodePointer midNode		= other._data.head;
		for (;;) {
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

	template<class Comp = std::less<>>
	void sort(Comp comp = Comp{}) noexcept {
		// Sort whole list using merge sort, elements are compared using comp
		this->_sort(_data.before_head(), comp);
	}

private:
	template<class... Args>
	void _construct(const size_type count, Args&&... args) {
		// Construct list using args
		if (count != 0) {
			ForwardListInsertOperation<_FwdListValue> insertOp;
			if constexpr (sizeof...(Args) == 0) {
				insertOp.append_n(count);
			}
			else if constexpr (sizeof...(Args) == 1) {
				insertOp.append_n(count, args...);
			}
			else if constexpr (sizeof...(Args) == 2) {
				insertOp.append_range(std::forward<Args>(args)...);
			}
			else {
				throw std::logic_error("Should be unreachable");
			}

			insertOp.attach_after(_data.before_head());
		}
	}

	template<class... Args>
	void _insert_after(_NodePointer node, Args&&... args) {
		// Insert after node by perfect forwarding args
		const _NodePointer newNode = static_cast<_NodePointer>(memory::allocate(1, sizeof(_NodeType)));
		try {
			memory::construct_in_place(newNode->value, std::forward<Args>(args)...);
		}
		catch(...) {
			// Rollback when node->value's constructor throws
			memory::deallocate(newNode, sizeof(_NodeType));
			throw;
		}

		memory::construct_in_place(newNode->next, node->next);
		node->next = newNode;
	}

	void _erase_after(_NodePointer node) noexcept {
		// Erase after node
		_NodeType::free_node(std::exchange(node->next, node->next->next)); // UB
	}

	void _splice_after(_NodePointer node, _NodePointer prevNode) noexcept {
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

	void _splice_after(_NodePointer node, _NodePointer first, _NodePointer last) noexcept {
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
	size_type _remove_if_after(UnaryPred pred, _NodePointer first, _NodePointer last) noexcept {
		// Remove elements satisfying pred in range (first, last)
		size_type removed = 0;
		for (_NodePointer currNode = first->next; currNode != last;) {
			if (pred(currNode->value)) { // UB
				_NodeType::free_node(std::exchange(first->next, currNode->next));
				++removed;
			} else {
				first = currNode;
			}
			currNode = first->next;
		}
		return removed;
	}

	template<class Comp>
	_NodePointer _merge(_NodePointer beforeFirst, _NodePointer beforeMid, _NodePointer beforeLast, Comp comp) noexcept {
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
			beforeFirst->next	= midNode;
			beforeMid->next		= nextNode;
			currNode->next		= firstNode;
			if (currNode == beforeLast) { // Second range is exhausted, return beforeMid
				return beforeMid;
			}
			// Advance node pointers for both ranges
			beforeFirst = firstNode;
			midNode = nextNode;
		}
	}

	template<class Comp>
	_NodePointer _sort(_NodePointer beforeFirst, size_type length, Comp comp) noexcept {
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

	template<class Comp>
	void _sort(_NodePointer beforeFirst, Comp comp) noexcept {
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
	_FwdListValue _data;
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
#endif // FORWARD_LIST_H