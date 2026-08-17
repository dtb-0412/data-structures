#pragma once
#ifndef FORWARD_LIST_H
#define FORWARD_LIST_H

#include<stdexcept>

#include"compare.hpp"
#include"memory.hpp"

template<class FwdListVal>
class ForwardListConstIterator {
private:
	using _NodePointer = typename FwdListVal::node_pointer;

public:
	using iterator_concept	= std::forward_iterator_tag;
	using iterator_category	= std::forward_iterator_tag;
	using value_type		= typename FwdListVal::value_type;
	using difference_type	= typename FwdListVal::difference_type;
	using pointer			= typename FwdListVal::const_pointer;
	using reference			= const value_type&;

	ForwardListConstIterator() noexcept
		: ptr() {}

	ForwardListConstIterator(_NodePointer ptr) noexcept
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
	using node_pointer	= ForwardListNode*;
	using value_type	= ValueT;

	ForwardListNode() = default;

	ForwardListNode(const ForwardListNode&)				= delete;
	ForwardListNode& operator=(const ForwardListNode&)	= delete;

	static void free_node(node_pointer node) noexcept {
		memory::destruct_at(std::addressof(node->next));
		memory::destruct_at(std::addressof(node->value));
		memory::deallocate(node, sizeof(ForwardListNode));
	}

	node_pointer	next; // Member next MUST come first.
	value_type		value;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class ForwardListValue {
public:
	using node_type			= NodeT;
	using node_pointer		= typename node_type::node_pointer;

	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;
	using reference			= value_type&;
	using const_reference	= const value_type&;

public:
	ForwardListValue() noexcept
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
			const_cast<node_pointer&>(head)					// Step 1: Const cast head to allow modification.
		)));
	}

	void clear() noexcept {
		auto currNode = std::exchange(head, nullptr);
		auto nextNode = node_pointer{};
		for (; currNode; currNode = nextNode) {
			nextNode = currNode->next;
			node_type::free_node(currNode);
		}
	}

	void swap(ForwardListValue& other) noexcept {
		using std::swap;
		swap(head, other.head);
	}

	node_pointer head;
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
	using _NodePointer	= typename _NodeType::node_pointer;

	using _FwdListValue	= ForwardListValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

	struct ForwardListInsertGuard {
		// Guard for list insertion failure
		ForwardListInsertGuard() noexcept
			: head(), tail() {}

		ForwardListInsertGuard(const ForwardListInsertGuard&)				= delete;
		ForwardListInsertGuard& operator=(const ForwardListInsertGuard&)	= delete;

		~ForwardListInsertGuard() {
			if (tail == _NodePointer{}) {
				return;
			}

			memory::construct_at(std::addressof(tail->next), _NodePointer{});
			while (head) {
				_NodeType::free_node(std::exchange(head, head->next));
			}
		}

		template<class... Args>
		void append_n(size_type count, const Args&... args) {
			// Append count elements by constructing in place using args
			if (count == 0) {
				return;
			}

			memory::NodeAllocateGuard<_NodeType> guard;
			if (tail == _NodePointer{}) {
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
			(void) guard.release();
		}

		template<class It, class Se>
		void append_range(It first, Se last) {
			// Append range [first, last)
			if (first == last) {
				return;
			}

			memory::NodeAllocateGuard<_NodeType> guard;
			if (tail == _NodePointer{}) {
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

				const auto newTail = guard.release();
				memory::construct_at(std::addressof(tail->next), newTail);
				tail = newTail;
			}
		}

		_NodePointer attach_after(_NodePointer node) noexcept {
			// Attach elements in *this after node, reset *this to default-initialized state
			const auto oldTail = tail;
			if (oldTail == _NodePointer{}) {
				return node;
			}

			memory::construct_at(std::addressof(oldTail->next), node->next);
			node->next = head;
			tail = _NodePointer{};

			return oldTail;
		}

		_NodePointer head; // Points to the first constructed node
		_NodePointer tail; // Points to the most recently constructed node
	};

	struct ForwardListRemoveGuard {
		/*
		This serves 2 purposes:
			1. RAII guard for list remove failure
			2. Queue for nodes waiting to be removed
		
		Instead of removing nodes while iterating, we queue them up and remove them all at once.
		
		Predicates for removal could be stateful, capturing references to other elements in the list,
		including ones that would be removed. Destructing nodes immediately after matching could invalidate
		the predicate's captured references, leading to undefined behavior.
		*/
		ForwardListRemoveGuard() noexcept
			: head(), tail(std::addressof(head)) {}

		ForwardListRemoveGuard(const ForwardListRemoveGuard&)				= delete;
		ForwardListRemoveGuard& operator=(const ForwardListRemoveGuard&)	= delete;

		~ForwardListRemoveGuard() {
			auto subject = head;
			while (subject) {
				const auto nextNode = subject->next;
				memory::destruct_at(std::addressof(subject->next));
				memory::destruct_at(std::addressof(subject->value));
				memory::deallocate(subject, sizeof(_NodeType));
				subject = nextNode;
			}
		}

		_NodePointer extract_after( _NodePointer prevNode) noexcept {
			// Extract node after prevNode from the list and add it to the remove queue
			const auto removed	= prevNode->next;
			const auto nextNode = removed->next;

			removed->next	= nullptr;
			prevNode->next	= nextNode;

			*tail	= removed;
			tail	= std::addressof(removed->next);
			return nextNode;
		}

		_NodePointer head;
		_NodePointer* tail;
	};

public:
	using iterator			= ForwardListIterator<_FwdListValue>;
	using const_iterator	= ForwardListConstIterator<_FwdListValue>;

public:
	ForwardList() noexcept
		: _data() {}

	explicit ForwardList(const size_type count)
		: _data() {
		this->_construct_n(count);
	}

	ForwardList(const size_type count, const T& val)
		: _data() {
		this->_construct_n(count, val);
	}

	template<std::input_iterator It>
		requires std::sentinel_for<It, It>
	ForwardList(It first, It last)
		: _data() {
		const auto count = static_cast<size_type>(std::distance(first, last));
		this->_construct_n(count, std::move(first), std::move(last));
	}

	ForwardList(std::initializer_list<T> initList)
		: _data() {
		this->_construct_n(initList.size(), initList.begin(), initList.end());
	}

	ForwardList(const ForwardList& other)
		: _data() {
		this->_construct_n(other.size(), other.begin(), other.end());
	}

	ForwardList(ForwardList&& other) noexcept
		: _data() {
		_data.head = std::exchange(other._data.head, nullptr);
	}

	~ForwardList() noexcept {
		_data.clear();
	}

	ForwardList& operator=(const ForwardList& other) {
		if (this != std::addressof(other)) {
			this->_assign_range(other.begin(), other.end());
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
		this->_assign_range(initList.begin(), initList.end());
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

	[[nodiscard]] size_type size() const noexcept {
		return static_cast<size_type>(std::distance(this->begin(), this->end()));
	}

	[[nodiscard]] size_type max_size() const noexcept {
		return static_cast<size_type>(-1) / sizeof(_NodeType);
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.head == nullptr;
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
	reference emplace_front(Args&&... args) {
		// Insert at begin by constructing in place using args
		this->_insert_after(_data.before_head(), std::forward<Args>(args)...);
		return this->front();
	}

	template<class... Args>
	iterator emplace_after(const_iterator where, Args&&... args) {
		// Insert after where by constructing in place using args
		this->_insert_after(where.ptr, std::forward<Args>(args)...);
		return iterator(where.ptr->next);
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
		if (count != 0) {
			ForwardListInsertGuard guard;
			guard.append_n(count, val);
			return iterator(guard.attach_after(where.ptr));
		}
		return iterator(where.ptr);
	}

	template<std::input_iterator It>
		requires std::sentinel_for<It, It>
	iterator insert_after(const_iterator where, It first, It last) {
		// Insert range [first, last) after where
		if (first != last) {
			ForwardListInsertGuard guard;
			guard.append_range(std::move(first), std::move(last));
			return iterator(guard.attach_after(where.ptr));
		}
		return iterator(where.ptr);
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

	template<std::input_iterator It>
		requires std::sentinel_for<It, It>
	void assign(It first, It last) {
		// Assign range [first, last)
		this->_assign_range(std::move(first), std::move(last));
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
		const auto currNode = first.ptr;
		const auto lastNode = last.ptr;
		if (currNode != lastNode) {
			for (;;) {
				const auto subject = currNode->next;
				if (subject == lastNode) {
					break;
				}

				currNode->next = subject->next;
				_NodeType::free_node(subject);
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

	void splice_after(const_iterator where, ForwardList<T>& other) noexcept {
		// Splice all of other after where
		if (this != std::addressof(other) && !other.is_empty()) {
			this->_splice_after(where, other.before_begin(), other.end());
		}
	}

	void splice_after(const_iterator where, ForwardList<T>&& other) noexcept {
		// Splice all of other after where
		this->splice_after(where, other);
	}

	void splice_after(const_iterator where, const_iterator first) noexcept {
		// Splice one node in range (first, first + 2) after where
		return this->_splice_after(where, first);
	}

	void splice_after(const_iterator where, const_iterator first, const_iterator last) noexcept {
		// Splice range (first, last) after where
		return this->_splice_after(where, first, last);
	}

	size_type remove(const T& val) {
		// Erase all elements matching val
		return this->remove_if([&](const T& other) -> bool { return other == val; });
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
		// Erase all duplicates
		return this->remove_adjacent_if(std::equal_to<>{});
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

	template<class Comp = std::less<>>
	void sort(Comp comp = Comp{}) {
		// Sort whole list using merge sort, elements are compared using comp
		this->_sort(_data.before_head(), comp);
	}

	void reverse() noexcept {
		// Reverse elements order
		if (this->is_empty()) {
			return;
		}

		auto prevNode = _NodePointer{};
		auto currNode = _data.head;
		auto nextNode = currNode->next;
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

private:
	template<class... Args>
	void _construct_n(const size_type count, Args&&... args) {
		// Construct list using args
		ForwardListInsertGuard guard;
		if constexpr (sizeof...(Args) == 0) {
			guard.append_n(count);
		}
		else if constexpr (sizeof...(Args) == 1) {
			guard.append_n(count, args...);
		}
		else if constexpr (sizeof...(Args) == 2) {
			guard.append_range(std::forward<Args>(args)...);
		}
		else {
			static_assert(false, "Unexpected number of arguments");
		}

		guard.attach_after(_data.before_head());
	}

	template<class... Args>
	void _insert_after(_NodePointer node, Args&&... args) {
		// Insert after node by perfect forwarding args
		memory::NodeAllocateGuard<_NodeType> guard;
		guard.allocate();
		memory::construct_at(std::addressof(guard.node->value), std::forward<Args>(args)...);
		memory::construct_at(std::addressof(guard.node->next), node->next);
		node->next = guard.release();
	}

	template<class It, class Se>
	void _assign_range(It first, Se last) {
		// Assign range [first, last)
		auto currNode = _data.before_head();
		for (; first != last; ++first) {
			const auto nextNode = currNode->next;
			if (!nextNode) {
				// Runs out of nodes, insert the remaining nodes to *this
				ForwardListInsertGuard guard;
				guard.append_range(first, last);
				guard.attach_after(currNode);
				return;
			}
			// Assign [first, last) to current nodes
			nextNode->value = *first;
			currNode = nextNode;
		}
		// Trim excessive nodes from *this
		for (auto subject = std::exchange(currNode->next, nullptr); subject;) {
			const auto nextNode = subject->next;
			_NodeType::free_node(subject);
			subject = nextNode;
		}
	}

	void _erase_after(_NodePointer node) noexcept {
		// Erase after node
		auto subject	= node->next;
		node->next		= subject->next;
		_NodeType::free_node(subject);
	}

	void _splice_after(const_iterator where, const_iterator first) noexcept {
		// Splice one node in range (first, first + 2) after where
		const auto whereNode	= where.ptr;
		const auto currNode		= first.ptr;

		if (whereNode != currNode) {
			const auto nextNode = currNode->next;
			if (whereNode != nextNode) {
				currNode->next	= nextNode->next;
				nextNode->next	= whereNode->next;
				whereNode->next = nextNode;
			}
		}
	}

	template<class Se>
	void _splice_after(const_iterator where, const_iterator first, Se last) noexcept {
		// Splice range (first, last) after node
		if (first == last) {
			return;
		}

		const auto whereNode	= where.ptr;
		const auto firstNode	= first.ptr;
		const auto lastNode		= last.ptr;
		// Find prev of last
		auto nextNode = firstNode->next;
		if (nextNode == lastNode) {
			return;
		}

		auto currNode = firstNode;
		do {
			currNode = nextNode;
			nextNode = nextNode->next;
		}
		while (nextNode != lastNode);
		// UB: if where is in range (first, last), this will lead to 2 unowned, circular node chains
		const auto extractedHead = firstNode->next;
		firstNode->next = nextNode;
		currNode->next	= whereNode->next;
		whereNode->next = extractedHead;
	}

	template<class UnaryPred, class Se>
	size_type _remove_if(UnaryPred pred, const_iterator first, Se last) {
		// Erase all elements matching pred in range (first, last)
		if (first == last) {
			return 0;
		}

		ForwardListRemoveGuard guard;

		size_type removed = 0;
		for (auto currNode = first.ptr, nextNode = currNode->next; nextNode != last.ptr;) {
			if (pred(nextNode->value)) {
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

	template<class BinaryPred, class Se>
	size_type _remove_adjacent_if(BinaryPred pred, const_iterator first, Se last) {
		// Erase all adjacent elements matching pred in range (first, last)
		if (first == last) {
			return 0;
		}

		ForwardListRemoveGuard guard;

		size_type removed = 0;
		for (auto currNode = first.ptr, nextNode = currNode->next; nextNode != last.ptr;) {
			if (pred(currNode->value, nextNode->value)) {
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
	void _merge(ForwardList<T>& other, Comp comp) {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		if (this == std::addressof(other) || other.is_empty()) {
			return;
		}

		if (this->is_empty()) {
			_data.head = std::exchange(other._data.head, nullptr);
			return;
		}

		auto beforeFirst	= _data.before_head();
		auto beforeMid		= other._data.before_head();
		auto midNode		= other._data.head;
		for (;;) {
			// Find position in the first range where insertion is needed
			_NodePointer firstNode{};
			while (true) {
				firstNode = beforeFirst->next;
				if (!firstNode) { // First range is exhausted, return
					beforeFirst->next	= midNode;
					other._data.head	= nullptr;
					return;
				}

				if (comp(midNode->value, firstNode->value)) {
					break;
				}
				beforeFirst = firstNode;
			}
			// Find sub-range in the second range to insert into the first range
			auto currNode = midNode;
			_NodePointer nextNode{};
			while (true) {
				nextNode = currNode->next;
				if (!nextNode) { // Second range is exhausted
					break;
				}

				if (!comp(nextNode->value, firstNode->value)) {
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
		auto midNode = beforeMid->next;
		for (;;) {
			// Find position in the first range where insertion is needed
			_NodePointer firstNode{};
			while (true) {
				firstNode = beforeFirst->next;
				if (beforeFirst == beforeMid) { // First range is exhausted, return beforeLast
					return beforeLast;
				}

				if (comp(midNode->value, firstNode->value)) {
					break;
				}
				beforeFirst = firstNode;
			}
			// Find sub-range in the second range to insert into the first range
			auto currNode = midNode;
			_NodePointer nextNode{};
			while (true) {
				nextNode = currNode->next;
				if (currNode == beforeLast) { // Second range is exhausted
					break;
				}

				if (!comp(nextNode->value, firstNode->value)) {
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
		const auto firstNode = beforeFirst->next;
		if (!firstNode) {
			return beforeFirst;
		}

		const auto lastNode = firstNode->next;
		if (!lastNode || comp(firstNode->value, lastNode->value)) {
			return firstNode;
		}
		// Swap firstNode and lastNode
		firstNode->next		= lastNode->next;
		beforeFirst->next	= lastNode;
		lastNode->next		= firstNode;
		return lastNode;
	}

	template<class Comp>
	_NodePointer _sort_base(_NodePointer beforeFirst, const size_type length, Comp comp) {
		// Sort range (beforeFirst, beforeFirst + length), or until nullptr is encountered
		if (length <= 2) {
			return this->_sort2(beforeFirst, comp);
		}
		// Sort top-down half length
		const auto halfLength	= length / 2;
		const auto beforeMid	= this->_sort_base(beforeFirst, halfLength, comp);
		if (!beforeMid->next) {
			return beforeMid;
		}

		const auto beforeLast = this->_sort_base(beforeMid, halfLength, comp);
		return this->_inplace_merge(beforeFirst, beforeMid, beforeLast, comp);
	}

	template<class Comp>
	void _sort(_NodePointer beforeFirst, Comp comp) noexcept {
		// Sort whole list bottom-up
		auto beforeMid = this->_sort2(beforeFirst, comp);
		
		size_type length = 2;
		do {
			if (!beforeMid->next) {
				return;
			}

			const auto beforeLast	= this->_sort_base(beforeMid, length, comp);
			beforeMid				= this->_inplace_merge(beforeFirst, beforeMid, beforeLast, comp);
			
			length <<= 1; // length *= 2
		}
		while (length != 0);
	}

private:
	_FwdListValue _data;
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
	return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{});
}
#endif // FORWARD_LIST_H