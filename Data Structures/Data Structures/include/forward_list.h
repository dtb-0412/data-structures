#pragma once
#ifndef FORWARD_LIST_H
#define FORWARD_LIST_H

#include"compare.hpp"
#include"memory.hpp"

template<class FwdListVal>
class _ForwardListConstIterator {
private:
	using _NodePointer = typename FwdListVal::node_pointer;

public:
	using iterator_concept	= std::forward_iterator_tag;
	using iterator_category	= std::forward_iterator_tag;
	using value_type		= typename FwdListVal::value_type;
	using difference_type	= typename FwdListVal::difference_type;
	using pointer			= typename FwdListVal::const_pointer;
	using reference			= const value_type&;

	_ForwardListConstIterator() noexcept
		: ptr() {}

	_ForwardListConstIterator(_NodePointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] reference operator*() const noexcept {
		return ptr->value;
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_ForwardListConstIterator& operator++() noexcept {
		ptr = ptr->next;
		return *this;
	}

	_ForwardListConstIterator operator++(int) noexcept {
		_ForwardListConstIterator temp = *this;
		ptr = ptr->next;
		return temp;
	}

	[[nodiscard]] bool operator==(const _ForwardListConstIterator& rhs) const noexcept {
		return ptr == rhs.ptr;
	}

public:
	_NodePointer ptr;
};

template<class FwdListVal>
class _ForwardListIterator : public _ForwardListConstIterator<FwdListVal> {
private:
	using _BaseIter	= _ForwardListConstIterator<FwdListVal>;
	using _BaseIter::_BaseIter;

public:
	using iterator_concept	= std::forward_iterator_tag;
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

	_ForwardListIterator operator++(int) noexcept {
		_ForwardListIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}
};

template<class ValueT>
struct _ForwardListNode {
	using node_pointer	= _ForwardListNode*;
	using value_type	= ValueT;

	_ForwardListNode() = default;

	_ForwardListNode(const _ForwardListNode&)				= delete;
	_ForwardListNode& operator=(const _ForwardListNode&)	= delete;

	static void free_node(node_pointer node) noexcept {
		memory::destruct_at(std::addressof(node->next));
		memory::destruct_at(std::addressof(node->value));
		memory::deallocate(node, sizeof(_ForwardListNode));
	}

	node_pointer	next;	// Member next MUST come first.
	value_type		value;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
struct _ForwardListValue {
	using node_type			= NodeT;
	using node_pointer		= typename node_type::node_pointer;

	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;

	_ForwardListValue() noexcept
		: head() {}

	[[nodiscard]] node_pointer before_head() const noexcept {
		// Returns pointer to "before head" pseudo node
		/*
		First, cast away head's constness, then reinterpret cast it from node_pointer to node_type&.
		After the reinterpret cast, head's memory location overlaps with member next of the "before head" pseudo node.
		The result is the pseudo node which points to head through its member next.
		Take the pseudo node's address, cast it to a node_pointer and we get a "before head" node_pointer.

		Important:
		The "before head" node_pointer can only be used as a sentinel for insertions to the front of the list.
		Only accessing the member next is valid, all other operations on the pointer (dereferencing, accessing its value, ...) are UB.
		*/
		return static_cast<node_pointer>(std::addressof(	// Step 3: Take the address of the pseudo node and cast it to a node_pointer.
			reinterpret_cast<node_type&>(					// Step 2: Reinterpret cast head from a node_pointer to a node_type&.
				const_cast<node_pointer&>(head)				// Step 1: Const cast head to allow modification.
			)
		));
	}

	void clear() noexcept {
		node_pointer currNode = std::exchange(head, nullptr);
		while (currNode) {
			const node_pointer nextNode = currNode->next;
			node_type::free_node(currNode);
			currNode = nextNode;
		}
	}

	void swap(_ForwardListValue& other) noexcept {
		using std::swap;
		swap(head, other.head);
	}

	node_pointer head;
};

template<class FwdListVal>
struct _ForwardListInsertGuard {
	// Guard for forward list insertion failure
	using node_type		= typename FwdListVal::node_type;
	using node_pointer	= typename FwdListVal::node_pointer;
	using size_type		= typename FwdListVal::size_type;

	_ForwardListInsertGuard(FwdListVal& data) noexcept
		: data(std::addressof(data)), head(), tail() {}

	_ForwardListInsertGuard(const _ForwardListInsertGuard&)				= delete;
	_ForwardListInsertGuard& operator=(const _ForwardListInsertGuard&)	= delete;

	~_ForwardListInsertGuard() {
		if (tail == node_pointer{}) {
			return;
		}

		memory::construct_at(std::addressof(tail->next), node_pointer{});
		while (head) {
			node_type::free_node(std::exchange(head, head->next));
		}
	}

	template<class... Args>
	void append_n(size_type count, const Args&... args) {
		// Append count elements by constructing in place using args
		if (count <= 0) {
			return;
		}

		memory::_NodeAllocateGuard<node_type> guard;
		if (tail == node_pointer{}) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), args...);
			head = guard.node;
			tail = guard.node;
			--count;
		}

		for (; 0 < count; --count) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), args...);
			memory::construct_at(std::addressof(tail->next), guard.node);
			tail = guard.node;
		}
		(void)guard.release();
	}

	template<class It, class Se>
	void append_range(It first, const Se last) {
		// Append range [first, last)
		if (first == last) {
			return;
		}

		memory::_NodeAllocateGuard<node_type> guard;
		if (tail == node_pointer{}) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), *first);

			const auto newHead = guard.release();
			head = newHead;
			tail = newHead;
			++first;
		}

		for (; first != last; ++first) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), *first);
			memory::construct_at(std::addressof(tail->next), guard.node);
			tail = guard.release();
		}
	}

	node_pointer attach_after(node_pointer node) noexcept {
		// Attach elements in *this after node, reset *this to default-initialized state
		const node_pointer oldTail = tail;
		if (oldTail == node_pointer{}) {
			return node;
		}

		memory::construct_at(std::addressof(oldTail->next), node->next);
		node->next = head;
		tail = node_pointer{};

		return oldTail;
	}

	node_pointer attach_head() noexcept {
		// Attach elements in *this at the beginning
		this->attach_after(data->before_head());
	}

	FwdListVal*		data;
	node_pointer	head; // Points to the first constructed node
	node_pointer	tail; // Points to the most recently constructed node
};

template<class NodeT>
struct _ForwardListRemoveGuard {
	/*
	Guard for forward list batch removal failure

	Instead of removing nodes while iterating, we queue them up and remove them all at once.

	Predicates for removal could be stateful, capturing references to other elements in the list,
	including ones that would be removed. Destructing nodes immediately after matching could invalidate
	the predicate's captured references, leading to undefined behavior.
	*/
	using node_type		= NodeT;
	using node_pointer	= typename node_type::node_pointer;

	_ForwardListRemoveGuard() noexcept
		: head(), tail(std::addressof(head)) {}

	_ForwardListRemoveGuard(const _ForwardListRemoveGuard&)				= delete;
	_ForwardListRemoveGuard& operator=(const _ForwardListRemoveGuard&)	= delete;

	~_ForwardListRemoveGuard() {
		while (head) {
			const node_pointer nextNode = head->next;
			node_type::free_node(head);
			head = nextNode;
		}
	}

	node_pointer extract_after(node_pointer node) noexcept {
		// Extract node after node from the list and add it to the remove queue
		const node_pointer removed	= node->next;
		const node_pointer nextNode = removed->next;

		removed->next	= nullptr;
		node->next		= nextNode;

		*tail	= removed;
		tail	= std::addressof(removed->next);
		return nextNode;
	}

	node_pointer	head;
	node_pointer*	tail;
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
	using _NodeType		= _ForwardListNode<T>;
	using _NodePointer	= typename _NodeType::node_pointer;

	using _MyVal		= _ForwardListValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

public:
	using iterator			= _ForwardListIterator<_MyVal>;
	using const_iterator	= _ForwardListConstIterator<_MyVal>;

public:
	ForwardList() noexcept
		: _data() {}

	explicit ForwardList(const size_type count)
		: _data() {
		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_n(count);
		guard.attach_head();
	}

	ForwardList(const size_type count, const T& val)
		: _data() {
		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_n(count, val);
		guard.attach_head();
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	ForwardList(It first, Se last)
		: _data() {
		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_range(std::move(first), std::move(last));
		guard.attach_head();
	}

	ForwardList(std::initializer_list<T> initList)
		: _data() {
		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_range(initList.begin(), initList.end());
		guard.attach_head();
	}

	ForwardList(const ForwardList& other)
		: _data() {
		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_range(other.begin(), other.end());
		guard.attach_head();
	}

	ForwardList(ForwardList&& other) noexcept
		: _data() {
		_data.swap(other._data);
	}

	~ForwardList() noexcept {
		_data.clear();
	}

	ForwardList& operator=(const ForwardList& other) {
		if (this != std::addressof(other)) {
			this->_assign(other.begin(), other.end());
		}
		return *this;
	}

	ForwardList& operator=(ForwardList&& other) noexcept {
		if (this != std::addressof(other)) {
			_data.clear();
			_data.swap(other._data);
		}
		return *this;
	}

	ForwardList& operator=(std::initializer_list<T> initList) {
		this->_assign(initList.begin(), initList.end());
		return *this;
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
		return _data.head->value; // UB: head could be nullptr
	}

	[[nodiscard]] const_reference front() const noexcept {
		return _data.head->value;
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.head == nullptr;
	}

	[[nodiscard]] size_type size() const noexcept {
		/*
		We have to trade off between having O(1) size query and O(1) splice. Whichever we choose, the other will be O(n).
		ForwardList is designed to be light, memory efficient and zero-overhead. Therefore, we sacrifice the size query
		for performance and efficiency.
		
		This operation is O(n) because we basically count every node in the list.
		*/
		return static_cast<size_type>(std::distance(this->begin(), this->end()));
	}

	[[nodiscard]] size_type max_size() const noexcept {
		return std::min(
			static_cast<size_type>(std::numeric_limits<difference_type>::max()),
			static_cast<size_type>(-1) / sizeof(_NodeType)
		);
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
	iterator emplace_after(const_iterator where, Args&&... args) {
		// Insert after where by constructing in place using args
		this->_insert_after(where.ptr, std::forward<Args>(args)...);
		return iterator(where.ptr->next);
	}

	template<class... Args>
	reference emplace_front(Args&&... args) {
		// Insert at begin by constructing in place using args
		this->_insert_after(_data.before_head(), std::forward<Args>(args)...);
		return this->front();
	}

	iterator insert_after(const_iterator where, const T& val) {
		// Insert after where by copying val
		this->_insert_after(where.ptr, val);
		return iterator(where.ptr->next);
	}

	iterator insert_after(const_iterator where, T&& val) {
		// Insert after where by copying val
		return this->emplace_after(where, std::move(val));
	}

	iterator insert_after(const_iterator where, const size_type count, const T& val) {
		// Insert count * val after where
		if (count == 0) {
			return iterator(where.ptr);
		}

		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_n(count, val);
		return iterator(guard.attach_after(where.ptr));
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	iterator insert_after(const_iterator where, It first, It last) {
		// Insert range [first, last) after where
		if (first == last) {
			return iterator(where.ptr);
		}

		_ForwardListInsertGuard<_MyVal> guard(_data);
		guard.append_range(std::move(first), std::move(last));
		return iterator(guard.attach_after(where.ptr));
	}

	iterator insert_after(const_iterator where, std::initializer_list<T> initList) {
		// Insert initList after where
		return this->insert_after(where, initList.begin(), initList.end());
	}

	void assign(const size_type count, const T& val) {
		// Assign count * val
		_data.clear();
		this->insert_after(this->before_begin(), count, val);
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

	void pop_front() noexcept {
		// Erase the first element
		this->_erase_after(_data.before_head());
	}

	iterator erase_after(const_iterator where) noexcept {
		// Erase after where
		this->_erase_after(where.ptr);
		return iterator(where.ptr->next);
	}

	iterator erase_after(const_iterator first, const_iterator last) noexcept {
		// Erase range (first, last)
		const _NodePointer currNode = first.ptr;
		const _NodePointer lastNode = last.ptr;
		if (currNode != lastNode) {
			while (true) {
				const _NodePointer nextNode = currNode->next;
				if (nextNode == lastNode) {
					break;
				}

				currNode->next = nextNode->next;
				_NodeType::free_node(nextNode);
			}
		}
		return iterator(lastNode);
	}

	void clear() noexcept {
		// Erase all elements
		_data.clear();
	}

	void swap(ForwardList& other) noexcept {
		// Swap with other
		if (this != std::addressof(other)) {
			_data.swap(other._data);
		}
	}

	void splice_after(const_iterator where, ForwardList& other) noexcept {
		// Splice all of other after where
		if (this != std::addressof(other) && !other.is_empty()) {
			this->_splice_after(where, other, other.before_begin(), other.end());
		}
	}

	void splice_after(const_iterator where, ForwardList&& other) noexcept {
		// Splice all of other after where
		this->splice_after(where, other);
	}

	void splice_after(const_iterator where, ForwardList& other, const_iterator first) noexcept {
		// Splice one node in range (first, first + 2) of other after where
		return this->_splice_after(where, other, first);
	}

	void splice_after(const_iterator where, ForwardList&& other, const_iterator first) noexcept {
		// Splice one node in range (first, first + 2) of other after where
		return this->_splice_after(where, other, first);
	}

	void splice_after(const_iterator where, ForwardList& other, const_iterator first, const_iterator last) noexcept {
		// Splice range (first, last) of other after where
		return this->_splice_after(where, other, first, last);
	}

	void splice_after(const_iterator where, ForwardList&& other, const_iterator first, const_iterator last) noexcept {
		// Splice range (first, last) of other after where
		return this->_splice_after(where, other, first, last);
	}

	size_type remove(const T& val) {
		// Erase all elements matching val
		return this->remove_if([&](const T& other) -> bool { return other == val; }, this->before_begin(), this->end());
	}

	size_type remove(const T& val, const_iterator first, const_iterator last) {
		// Erase all elements matching val in range (first, last)
		return this->remove_if([&](const T& other) -> bool { return other == val; }, first, last);
	}

	template<class UnaryPred>
	size_type remove_if(UnaryPred pred) {
		// Erase all elements matching pred
		return this->remove_if(pred, this->before_begin(), this->end());
	}

	template<class UnaryPred>
	size_type remove_if(UnaryPred pred, const_iterator first, const_iterator last) {
		// Erase all elements matching pred in range (first, last)
		return this->_remove_if(pred, first, last);
	}

	template<class BinaryPred>
	size_type remove_adjacent_if(BinaryPred pred) {
		// Erase all adjacent elements matching pred
		return this->remove_adjacent_if(pred, this->before_begin(), this->end());
	}

	template<class BinaryPred>
	size_type remove_adjacent_if(BinaryPred pred, const_iterator first, const_iterator last) {
		// Erase all adjacent elements matching pred in range (first, last)
		return this->_remove_adjacent_if(pred, first, last);
	}

	size_type unique() {
		// Erase all adjacent duplicates
		return this->remove_adjacent_if(std::equal_to<>{}, this->before_begin(), this->end());
	}

	size_type unique(const_iterator first, const_iterator last) {
		// Erase all adjacent duplicates in range (first, last)
		return this->remove_adjacent_if(std::equal_to<>{}, first, last);
	}

	void reverse() noexcept {
		// Reverse elements order
		return this->reverse(this->before_begin(), this->end());
	}

	void reverse(const_iterator first, const_iterator last) noexcept {
		// Reverse elements order in range (first, last)
		if (first == last) {
			return;
		}

		_NodePointer prevNode = last.ptr;
		_NodePointer currNode = first.ptr->next;
		if (prevNode == currNode) {
			return;
		}

		_NodePointer nextNode = currNode->next;
		while (true) {
			currNode->next = prevNode;
			if (!nextNode) {
				first.ptr->next = currNode;
				return;
			}

			prevNode = currNode;
			currNode = nextNode;
			nextNode = nextNode->next;
		}
	}

	void merge(ForwardList& other) {
		// Merge with other, assuming both lists are sorted and elements are compared using operator<
		this->_merge(other, std::less<>{});
	}

	void merge(ForwardList&& other) {
		// Merge with other, assuming both lists are sorted and elements are compared using operator<
		this->_merge(other, std::less<>{});
	}

	template<class Comp>
	void merge(ForwardList& other, Comp comp) {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		this->_merge(other, comp);
	}

	template<class Comp>
	void merge(ForwardList&& other, Comp comp) {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		this->_merge(other, comp);
	}

	void sort() {
		// Sort whole list using merge sort, elements are compared using std::less
		this->_sort(_data.before_head(), std::less<>{});
	}

	template<class Comp>
	void sort(Comp comp) {
		// Sort whole list using merge sort, elements are compared using comp
		this->_sort(_data.before_head(), comp);
	}

private:
	template<class... Args>
	void _insert_after(_NodePointer node, Args&&... args) {
		// Insert after node by perfect forwarding args
		memory::_NodeAllocateGuard<_NodeType> guard;
		guard.allocate();
		memory::construct_at(std::addressof(guard.node->value), std::forward<Args>(args)...);
		memory::construct_at(std::addressof(guard.node->next), node->next);
		node->next = guard.release();
	}

	template<class It, class Se>
	void _assign(It first, const Se last) {
		// Assign range [first, last)
		_NodePointer currNode = _data.before_head();
		for (; first != last; ++first) {
			const _NodePointer nextNode = currNode->next;
			if (!nextNode) {
				// Runs out of nodes, insert the remaining nodes to *this
				_ForwardListInsertGuard<_MyVal> guard(_data);
				guard.append_range(first, last);
				guard.attach_after(currNode);
				return;
			}
			// Assign [first, last) to current nodes
			nextNode->value = *first;
			currNode = nextNode;
		}
		// Trim excessive nodes from *this
		for (_NodePointer subject = std::exchange(currNode->next, nullptr); subject;) {
			const _NodePointer nextNode = subject->next;
			_NodeType::free_node(subject);
			subject = nextNode;
		}
	}

	void _erase_after(_NodePointer node) noexcept {
		// Erase after node
		_NodePointer subject = node->next;
		node->next = subject->next;
		_NodeType::free_node(subject);
	}

	void _splice_after(const_iterator where, ForwardList& other, const_iterator first) noexcept {
		// Splice one node in range (first, first + 2) after where
		(void)other;

		const _NodePointer whereNode	= where.ptr;
		const _NodePointer currNode		= first.ptr;

		if (whereNode != currNode) {
			const _NodePointer nextNode = currNode->next;
			if (whereNode != nextNode) {
				currNode->next	= nextNode->next;
				nextNode->next	= whereNode->next;
				whereNode->next = nextNode;
			}
		}
	}

	void _splice_after(const_iterator where, ForwardList& other, const_iterator first, const_iterator last) noexcept {
		// Splice range (first, last) after node
		(void)other;

		if (first == last) {
			return;
		}

		const _NodePointer whereNode	= where.ptr;
		const _NodePointer firstNode	= first.ptr;
		const _NodePointer lastNode		= last.ptr;
		// Find prev of last
		_NodePointer nextNode = firstNode->next;
		if (nextNode == lastNode) {
			return;
		}

		_NodePointer currNode = firstNode;
		do {
			currNode = nextNode;
			nextNode = nextNode->next;
		}
		while (nextNode != lastNode);
		// UB: if where is in range (first, last), this will lead to 2 unowned, circular node chains
		const _NodePointer extractedHead = firstNode->next;
		firstNode->next = nextNode;
		currNode->next	= whereNode->next;
		whereNode->next = extractedHead;
	}

	template<class UnaryPred>
	size_type _remove_if(UnaryPred pred, const_iterator first, const_iterator last) {
		// Erase all elements matching pred in range (first, last)
		const _NodePointer lastNode = last.ptr;

		_NodePointer prevNode = first.ptr;
		if (prevNode == lastNode) {
			return 0;
		}

		size_type removed = 0;
		_ForwardListRemoveGuard<_NodeType> guard;
		for (_NodePointer currNode = prevNode->next; currNode != lastNode;) {
			if (pred(currNode->value)) {
				currNode = guard.extract_after(prevNode);
				++removed;
			}
			else {
				prevNode = currNode;
				currNode = prevNode->next;
			}
		}
		return removed;
	}

	template<class BinaryPred>
	size_type _remove_adjacent_if(BinaryPred pred, const_iterator first, const_iterator last) {
		// Erase all adjacent elements matching pred in range (first, last)
		if (first == last) {
			return 0;
		}

		const _NodePointer lastNode = last.ptr;

		_NodePointer currNode = first.ptr->next;
		if (currNode == lastNode) {
			return 0;
		}

		size_type removed = 0;
		_ForwardListRemoveGuard<_NodeType> guard;
		for (_NodePointer nextNode = currNode->next; nextNode != lastNode;) {
			if (static_cast<bool>(pred(currNode->value, nextNode->value))) {
				nextNode = guard.extract_after(currNode);
				++removed;
			}
			else {
				currNode = nextNode;
				nextNode = currNode->next;
			}
		}
		return removed;
	}

	template<class Comp>
	void _merge(ForwardList& other, Comp comp) {
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
		while (true) {
			// Find position in the first range where insertion is needed
			_NodePointer firstNode;
			while (true) {
				firstNode = beforeFirst->next;
				if (!firstNode) { // First range is exhausted, return
					beforeFirst->next	= midNode;
					other._data.head	= nullptr;
					return;
				}

				if (static_cast<bool>(comp(midNode->value, firstNode->value))) {
					break;
				}
				beforeFirst = firstNode;
			}
			// Find sub-range in the second range to insert into the first range
			_NodePointer currNode = midNode;
			_NodePointer nextNode;
			while (true) {
				nextNode = currNode->next;
				if (!nextNode) { // Second range is exhausted
					break;
				}

				if (!static_cast<bool>(comp(nextNode->value, firstNode->value))) {
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

	template<class Comp>
	_NodePointer _inplace_merge(_NodePointer beforeFirst, _NodePointer beforeMid, _NodePointer beforeLast, Comp comp) noexcept {
		// Merge 2 sorted ranges (beforeFirst, beforeMid] and (beforeMid, beforeLast], both are in *this
		_NodePointer midNode = beforeMid->next;
		while (true) {
			// Find position in the first range where insertion is needed
			_NodePointer firstNode;
			while (true) {
				firstNode = beforeFirst->next;
				if (beforeFirst == beforeMid) { // First range is exhausted, return beforeLast
					return beforeLast;
				}

				if (static_cast<bool>(comp(midNode->value, firstNode->value))) {
					break;
				}
				beforeFirst = firstNode;
			}
			// Find sub-range in the second range to insert into the first range
			_NodePointer currNode = midNode;
			_NodePointer nextNode;
			while (true) {
				nextNode = currNode->next;
				if (currNode == beforeLast) { // Second range is exhausted
					break;
				}

				if (!static_cast<bool>(comp(nextNode->value, firstNode->value))) {
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
	_NodePointer _sort2(_NodePointer beforeFirst, Comp comp) {
		// Sort range (beforeFirst, beforeFirst + 2], or until nullptr is encountered
		const _NodePointer firstNode = beforeFirst->next;
		if (!firstNode) {
			return beforeFirst;
		}

		const _NodePointer lastNode = firstNode->next;
		if (!lastNode || static_cast<bool>(comp(firstNode->value, lastNode->value))) {
			return firstNode;
		}
		// Swap firstNode and lastNode
		firstNode->next		= lastNode->next;
		beforeFirst->next	= lastNode;
		lastNode->next		= firstNode;
		return lastNode;
	}

	template<class Comp>
	_NodePointer _sort(_NodePointer beforeFirst, const size_type length, Comp comp) {
		// Sort range (beforeFirst, beforeFirst + length), or until nullptr is encountered
		if (length <= 2) {
			return this->_sort2(beforeFirst, comp);
		}
		// Sort top-down half length
		const auto halfLength = length / 2;

		const _NodePointer beforeMid = this->_sort(beforeFirst, halfLength, comp);
		if (!beforeMid->next) {
			return beforeMid;
		}

		const _NodePointer beforeLast = this->_sort(beforeMid, halfLength, comp);
		return this->_inplace_merge(beforeFirst, beforeMid, beforeLast, comp);
	}

	template<class Comp>
	void _sort(_NodePointer beforeFirst, Comp comp) noexcept {
		// Sort whole list bottom-up
		_NodePointer beforeMid = this->_sort2(beforeFirst, comp);
		
		size_type length = 2;
		do {
			if (!beforeMid->next) {
				return;
			}

			const _NodePointer beforeLast	= this->_sort(beforeMid, length, comp);
			beforeMid						= this->_inplace_merge(beforeFirst, beforeMid, beforeLast, comp);
			
			length <<= 1; // length *= 2
		}
		while (length != 0);
	}

private:
	_MyVal _data;
};

template<class T>
void swap(ForwardList<T>& lhs, ForwardList<T>& rhs) noexcept {
	lhs.swap(rhs);
}

template<class T>
[[nodiscard]] bool operator==(const ForwardList<T>& lhs, const ForwardList<T>& rhs) {
	return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const ForwardList<T>& lhs, const ForwardList<T>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // FORWARD_LIST_H