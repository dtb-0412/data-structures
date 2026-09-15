#pragma once
#ifndef LIST_HPP
#define LIST_HPP

#include"compare.hpp"
#include"memory.hpp"

template<class ListVal>
class _ListConstIterator {
private:
	using _NodePointer = typename ListVal::node_pointer;

public:
	using iterator_concept	= std::bidirectional_iterator_tag;
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename ListVal::value_type;
	using difference_type	= typename ListVal::difference_type;
	using pointer			= typename ListVal::const_pointer;
	using reference			= const value_type&;

	_ListConstIterator() noexcept
		: ptr() {}

	_ListConstIterator(_NodePointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] reference operator*() const noexcept {
		return ptr->value;
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_ListConstIterator& operator++() noexcept {
		ptr = ptr->next;
		return *this;
	}

	_ListConstIterator operator++(int) noexcept {
		_ListConstIterator temp = *this;
		ptr = ptr->next;
		return temp;
	}

	_ListConstIterator& operator--() noexcept {
		ptr = ptr->prev;
		return *this;
	}

	_ListConstIterator operator--(int) noexcept {
		_ListConstIterator temp = *this;
		ptr = ptr->prev;
		return temp;
	}

	[[nodiscard]] bool operator==(const _ListConstIterator& rhs) const noexcept {
		return ptr == rhs.ptr;
	}

public:
	_NodePointer ptr;
};

template<class ListVal>
class _ListIterator : public _ListConstIterator<ListVal> {
private:
	using _BaseIter = _ListConstIterator<ListVal>;
	using _BaseIter::_BaseIter;

public:
	using iterator_concept	= std::bidirectional_iterator_tag;
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename ListVal::value_type;
	using difference_type	= typename ListVal::difference_type;
	using pointer			= typename ListVal::pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_ListIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	_ListIterator operator++(int) noexcept {
		_ListIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	_ListIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	_ListIterator operator--(int) noexcept {
		_ListIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}
};

template<class ValueT>
struct _ListNode {
	using node_pointer	= _ListNode*;
	using value_type	= ValueT;

	_ListNode() = default;

	_ListNode(const _ListNode&)				= delete;
	_ListNode& operator=(const _ListNode&)	= delete;

	[[nodiscard]] static node_pointer construct_head() {
		// Construct empty head sentinel with no value
		const auto newHead = static_cast<node_pointer>(memory::allocate(1, sizeof(_ListNode)));
		memory::construct_at(std::addressof(newHead->next), newHead);
		memory::construct_at(std::addressof(newHead->prev), newHead);
		return newHead;
	}

	static void free_empty_node(node_pointer node) noexcept {
		// Destroy pointer members and deallocate node memory
		memory::destruct_at(std::addressof(node->next));
		memory::destruct_at(std::addressof(node->prev));
		memory::deallocate(node, sizeof(_ListNode));
	}

	static void free_node(node_pointer node) noexcept {
		// Destroy entire node, along with its value
		memory::destruct_at(std::addressof(node->value));
		free_empty_node(node);
	}

	node_pointer	next;	// Successor node
	node_pointer	prev;	// Predecessor node
	value_type		value;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
struct _ListValue {
	using node_type		= NodeT;
	using node_pointer	= typename node_type::node_pointer;

	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;

	_ListValue() noexcept
		: head(), size(0) {}
	
	void clear_non_head() noexcept {
		head->prev->next = nullptr; // Remove circular link

		for (node_pointer currNode = head->next; currNode;) {
			const node_pointer nextNode = currNode->next;
			node_type::free_node(currNode);
			currNode = nextNode;
		}
	}

	void clear() noexcept {
		this->clear_non_head();
		node_type::free_empty_node(head);
	}

	void swap(_ListValue& other) noexcept {
		using std::swap;
		swap(head, other.head);
		swap(size, other.size);
	}

	node_pointer	head;
	size_type		size;
};

template<class ListVal>
struct _ListInsertGuard {
	// Guard for list insertion failure
	using node_type		= typename ListVal::node_type;
	using node_pointer	= typename ListVal::node_pointer;
	using value_type	= typename ListVal::value_type;
	using size_type		= typename ListVal::size_type;

	_ListInsertGuard(ListVal& data) noexcept
		: data(std::addressof(data)), head(), tail(), inserted(0) {}

	_ListInsertGuard(const _ListInsertGuard&)				= delete;
	_ListInsertGuard& operator=(const _ListInsertGuard&)	= delete;

	~_ListInsertGuard() {
		if (inserted == 0) {
			return;
		}

		memory::construct_at(std::addressof(head->prev), node_pointer{});
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
		if (inserted == 0) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), args...);
			head = guard.node;
			tail = guard.node;
			++inserted;
			--count;
		}

		for (; 0 < count; --count) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), args...);
			memory::construct_at(std::addressof(tail->next), guard.node);
			memory::construct_at(std::addressof(guard.node->prev), tail);
			tail = guard.node;
			++inserted;
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
		if (inserted == 0) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), *first);

			const auto newHead = guard.release();
			head = newHead;
			tail = newHead;
			++inserted;
			++first;
		}

		for (; first != last; ++first) {
			guard.allocate();
			memory::construct_at(std::addressof(guard.node->value), *first);
			memory::construct_at(std::addressof(tail->next), guard.node);
			memory::construct_at(std::addressof(guard.node->prev), tail);
			tail = guard.release();
			++inserted;
		}
	}

	node_pointer attach_before(node_pointer node) noexcept {
		// Attach elements in *this before node
		if (inserted == 0) {
			return node;
		}

		memory::construct_at(std::addressof(head->prev), node->prev);
		node->prev->next = head;
		memory::construct_at(std::addressof(tail->next), node);
		node->prev = tail;

		data->size += std::exchange(inserted, 0);
		return head;
	}

	void attach_at_end() noexcept {
		this->attach_before(data->head);
	}

	void attach_head() {
		memory::_NodeAllocateGuard<node_type> guard;
		guard.allocate();
		
		if (inserted == 0) {
			memory::construct_at(std::addressof(guard.node->next), guard.node);
			memory::construct_at(std::addressof(guard.node->prev), guard.node);
		}
		else {
			memory::construct_at(std::addressof(guard.node->next), head);
			memory::construct_at(std::addressof(guard.node->prev), tail);
			memory::construct_at(std::addressof(head->prev), guard.node);
			memory::construct_at(std::addressof(tail->next), guard.node);
		}

		data->head = guard.release();
		data->size = std::exchange(inserted, 0);
	}

	ListVal*		data;
	node_pointer	head;		// Points to the first constructed node
	node_pointer	tail;		// Points to the most recently constructed node
	size_type		inserted;	// Number of inserted nodes
};

template<class ListVal>
struct _ListRemoveGuard {
	// Guard for list batch removal failure
	using node_type		= typename ListVal::node_type;
	using node_pointer	= typename node_type::node_pointer;

	_ListRemoveGuard(ListVal& data) noexcept
		: data(std::addressof(data)), head(), tail(std::addressof(head)) {}

	_ListRemoveGuard(const _ListRemoveGuard&)				= delete;
	_ListRemoveGuard& operator=(const _ListRemoveGuard&)	= delete;

	~_ListRemoveGuard() {
		while (head) {
			const node_pointer nextNode = head->next;
			node_type::free_node(head);
			head = nextNode;
		}
	}

	node_pointer extract(node_pointer node) noexcept {
		// Extract node from the list and add it to the remove queue
		const node_pointer nextNode = std::exchange(node->next, nullptr);
		const node_pointer prevNode = node->prev;

		prevNode->next = nextNode;
		nextNode->prev = prevNode;

		--data->size;

		*tail	= node;
		tail	= std::addressof(node->next);
		return nextNode;
	}

	ListVal*		data;
	node_pointer	head;
	node_pointer*	tail;
};

template<class T>
class List {
public:
	using value_type		= T;
	using size_type			= std::size_t;
	using difference_type	= std::ptrdiff_t;
	using pointer			= T*;
	using const_pointer		= const T*;
	using reference			= T&;
	using const_reference	= const T&;

private:
	using _NodeType		= _ListNode<T>;
	using _NodePointer	= typename _NodeType::node_pointer;

	using _MyVal = _ListValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

public:
	using iterator			= _ListIterator<_MyVal>;
	using const_iterator	= _ListConstIterator<_MyVal>;

	using reverse_iterator			= std::reverse_iterator<iterator>;
	using const_reverse_iterator	= std::reverse_iterator<const_iterator>;

public:
	List() noexcept
		: _data() {
		_data.head = _NodeType::construct_head();
	}

	explicit List(const size_type count)
		: _data() {
		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_n(count);
		guard.attach_head();
	}

	List(const size_type count, const T& value)
		: _data() {
		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_n(count, value);
		guard.attach_head();
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	List(It first, Se last)
		: _data() {
		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_range(std::move(first), std::move(last));
		guard.attach_head();
	}

	List(std::initializer_list<T> initList)
		: _data() {
		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_range(initList.begin(), initList.end());
		guard.attach_head();
	}

	List(const List& other)
		: _data() {
		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_range(other.begin(), other.end());
		guard.attach_head();
	}

	List(List&& other) noexcept
		: _data() {
		_data.head = _NodeType::construct_head();
		_data.swap(other._data);
	}

	~List() noexcept {
		_data.clear();
	}

	List& operator=(const List& other) {
		if (this != std::addressof(other)) {
			this->assign(other.begin(), other.end());
		}
		return *this;
	}

	List& operator=(List&& other) noexcept {
		if (this != std::addressof(other)) {
			this->clear();
			_data.swap(other._data);
		}
		return *this;
	}

	List& operator=(std::initializer_list<T> initList) {
		this->assign(initList.begin(), initList.end());
		return *this;
	}

	[[nodiscard]] iterator begin() noexcept {
		return iterator(_data.head->next);
	}

	[[nodiscard]] const_iterator begin() const noexcept {
		return const_iterator(_data.head->next);
	}

	[[nodiscard]] iterator end() noexcept {
		return iterator(_data.head);
	}

	[[nodiscard]] const_iterator end() const noexcept {
		return const_iterator(_data.head);
	}

	[[nodiscard]] const_iterator cbegin() const noexcept {
		return this->begin();
	}

	[[nodiscard]] const_iterator cend() const noexcept {
		return this->end();
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

	[[nodiscard]] reference front() noexcept {
		return _data.head->next->value;
	}

	[[nodiscard]] const_reference front() const noexcept {
		return _data.head->next->value;
	}

	[[nodiscard]] reference back() noexcept {
		return _data.head->prev->value;
	}

	[[nodiscard]] const_reference back() const noexcept {
		return _data.head->prev->value;
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.size == 0;
	}

	[[nodiscard]] size_type size() const noexcept {
		return _data.size;
	}

	[[nodiscard]] size_type max_size() const noexcept {
		return std::min(
			static_cast<size_type>(std::numeric_limits<difference_type>::max()),
			static_cast<size_type>(-1) / sizeof(_NodeType)
		);
	}

	void push_front(const T& val) {
		// Insert by copying val at beginning
		this->_emplace(_data.head->next, val);
	}

	void push_front(T&& val) {
		// Insert by moving val at beginning
		this->_emplace(_data.head->next, std::move(val));
	}

	void push_back(const T& val) {
		// Insert by copying val at end
		this->_emplace(_data.head, val);
	}

	void push_back(T&& val) {
		// Insert by moving val at end
		this->_emplace(_data.head, std::move(val));
	}

	template<class... Args>
	iterator emplace(const_iterator where, Args&&... args) {
		// Insert by perfectly forwarding args into element at where
		return iterator(this->_emplace(where.ptr, std::forward<Args>(args)...));
	}

	template<class... Args>
	reference emplace_front(Args&&... args) {
		// Insert by perfectly forwarding args into element at beginning
		return this->_emplace(_data.head->next, std::forward<Args>(args)...)->value;
	}

	template<class... Args>
	reference emplace_back(Args&&... args) {
		// Insert by perfectly forwarding args into element at end
		return this->_emplace(_data.head, std::forward<Args>(args)...)->value;
	}

	iterator insert(const_iterator where, const T& val) {
		// Insert by moving val at where
		return this->emplace(where, val);
	}

	iterator insert(const_iterator where, T&& val) {
		// Insert by moving val at where
		return this->emplace(where, std::move(val));
	}

	iterator insert(const_iterator where, const size_type count, const T& val) {
		// Insert count * val at where
		if (count == 0) {
			return iterator(where.ptr);
		}

		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_n(count, val);
		return iterator(guard.attach_before(where.ptr));
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	iterator insert(const_iterator where, It first, Se last) {
		// Insert range [first, last) at where
		if (first == last) {
			return iterator(where.ptr);
		}

		_ListInsertGuard<_MyVal> guard(_data);
		guard.append_range(std::move(first), std::move(last));
		return iterator(guard.attach_before(where.ptr));
	}

	iterator insert(const_iterator where, std::initializer_list<T> initList) {
		// Insert range [initList.begin(), initList.end())
		return this->insert(where, initList.begin(), initList.end());
	}

	void assign(const size_type count, const T& val) {
		// Assign count * val
		const _NodePointer lastNode = _data.head;
		for (_NodePointer currNode = lastNode->next;;) {
			if (currNode == lastNode) {
				_ListInsertGuard<_MyVal> guard(_data);
				guard.append_n(count, val);
				guard.attach_at_end();
				return;
			}

			if (count == 0) {
				this->_erase(currNode, lastNode);
				return;
			}

			currNode->value = val;
			currNode = currNode->next;
			--count;
		}
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	void assign(It first, Se last) {
		// Assign range [first, last)
		this->_assign(std::move(first), std::move(last));
	}

	void assign(std::initializer_list<T> initList) {
		// Assign range [initList.begin(), initList.end())
		this->assign(initList.begin(), initList.end());
	}

	void pop_front() noexcept {
		// Erase the first element
		this->_erase(_data.head->next);
	}

	void pop_back() noexcept {
		// Erase the last element
		this->_erase(_data.head->prev);
	}

	iterator erase(const_iterator where) noexcept {
		// Erase element at where
		return iterator(this->_erase(where.ptr));
	}

	iterator erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		return iterator(this->_erase(first.ptr, last.ptr));
	}

	void clear() noexcept {
		// Erase all elements
		_data.clear_non_head();
		_data.head->next = _data.head;
		_data.head->prev = _data.head;
		_data.size = 0;
	}

	void swap(List& other) noexcept {
		// Swap with other
		using std::swap;
		if (this != std::addressof(other)) {
			_data.swap(other._data);
		}
	}

	void splice(const_iterator where, List& other) {
		// Splice all of other at where
		auto& otherData = other._data;
		if (this != std::addressof(other) && otherData.size != 0) {
			const auto otherHead = otherData.head;
			this->_splice(where.ptr, other, otherHead->next, otherHead, otherData.size);
		}
	}

	void splice(const_iterator where, List&& other) {
		// Splice all of other at where
		this->splice(where, other);
	}

	void splice(const_iterator where, List& other, const_iterator first) {
		// Splice one node in range [first, first + 1) of other at where
		const _NodePointer whereNode	= where.ptr;
		const _NodePointer firstNode	= first.ptr;
		const _NodePointer lastNode		= firstNode->next;
		if (this != std::addressof(other) || (whereNode != firstNode && whereNode != lastNode)) {
			this->_splice(whereNode, other, firstNode, lastNode, 1);
		}
	}

	void splice(const_iterator where, List&& other, const_iterator first) {
		// Splice one node in range [first, first + 1) of other at where
		this->splice(where, other, first);
	}

	void splice(const_iterator where, List& other, const_iterator first, const_iterator last) {
		// Splice range [first, last) of other at where
		const _NodePointer whereNode	= where.ptr;
		const _NodePointer firstNode	= first.ptr;
		const _NodePointer lastNode		= last.ptr;
		if (firstNode != lastNode && (this != std::addressof(other) || whereNode != lastNode)) {
			size_type count = 0;
			if (this != std::addressof(other)) {
				const auto otherLast = other._data.head;
				if (firstNode == otherLast->next && lastNode == otherLast) {
					count = other._data.size; // Splice whole other
				}
				else {
					for (_NodePointer _ = firstNode; _ != lastNode; _ = _->next) {
						++count; // Count nodes and check for knot
					}
				}
			}

			this->_splice(whereNode, other, firstNode, lastNode, count);
		}
	}

	void splice(const_iterator where, List&& other, const_iterator first, const_iterator last) {
		// Splice range [first, last) of other at where
		this->splice(where, other, first, last);
	}

	size_type remove(const T& val) {
		// Erase all elements matching val
		return this->remove_if([&](const T& other) -> bool { return other == val; }, this->begin(), this->end());
	}

	size_type remove(const T& val, const_iterator first, const_iterator last) {
		// Erase all elements matching val in range [first, last)
		return this->remove_if([&](const T& other) -> bool { return other == val; }, first, last);
	}

	template<class UnaryPred>
	size_type remove_if(UnaryPred pred) {
		// Erase all elements matching pred
		return this->remove_if(pred, this->begin(), this->end());
	}
	
	template<class UnaryPred>
	size_type remove_if(UnaryPred pred, const_iterator first, const_iterator last) {
		// Erase all elements matching pred in range [first, last)
		return this->_remove_if(pred, first, last);
	}

	template<class BinaryPred>
	size_type remove_adjacent_if(BinaryPred pred) {
		// Erase all adjacent elements matching pred
		return this->remove_adjacent_if(pred, this->begin(), this->end());
	}

	template<class BinaryPred>
	size_type remove_adjacent_if(BinaryPred pred, const_iterator first, const_iterator last) {
		// Erase all adjacent elements matching pred in range [first, last)
		return this->_remove_adjacent_if(pred, first, last);
	}

	size_type unique() {
		// Erase all adjacent duplicates
		return this->remove_adjacent_if(std::equal_to<>{}, this->begin(), this->end());
	}

	size_type unique(const_iterator first, const_iterator last) {
		// Erase all adjacent duplicates in range [first, last)
		return this->remove_adjacent_if(std::equal_to<>{}, first, last);
	}

	void reverse() noexcept {
		this->reverse(this->begin(), this->end());
	}

	void reverse(const_iterator first, const_iterator last) noexcept {
		// Reverse elements order in range [first, last)
		const _NodePointer firstNode	= first.ptr;
		const _NodePointer lastNode		= last.ptr;
		if (firstNode == lastNode) {
			return;
		}

		const _NodePointer beforeFirst	= firstNode->prev;
		const _NodePointer beforeLast	= lastNode->prev;

		for (_NodePointer currNode = firstNode;;) {
			const _NodePointer nextNode = currNode->next;
			currNode->next = currNode->prev;
			currNode->prev = nextNode;

			if (nextNode == lastNode) {
				break;
			}
			currNode = nextNode;
		}

		beforeFirst->next	= beforeLast;
		beforeLast->prev	= beforeFirst;

		firstNode->next = lastNode;
		lastNode->prev	= firstNode;
	}

	void merge(List& other) {
		// Merge with other, assuming both lists are sorted and elements are compared using operator<
		this->_merge(other, std::less<>{});
	}

	void merge(List&& other) {
		// Merge with other, assuming both lists are sorted and elements are compared using operator<
		this->_merge(other, std::less<>{});
	}

	template<class Comp>
	void merge(List& other, Comp comp) {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		this->_merge(other, comp);
	}

	template<class Comp>
	void merge(List&& other, Comp comp) {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		this->_merge(other, comp);
	}

	void sort() {
		// Sort whole list using merge sort, elements are compared using std::less
		this->_sort(_data.head->next, _data.size, std::less<>{});
	}

	template<class Comp>
	void sort(Comp comp) {
		// Sort whole list using merge sort, elements are compared using comp
		this->_sort(_data.head->next, _data.size, comp);
	}

private:
	template<class... Args>
	_NodePointer _emplace(_NodePointer node, Args&&... args) {
		// Insert before node by perfect forwarding args
		if (_data.size == this->max_size()) {
			this->_length_error();
		}

		memory::_NodeAllocateGuard<_NodeType> guard;
		guard.allocate();
		memory::construct_at(std::addressof(guard.node->value), std::forward<Args>(args)...);
		memory::construct_at(std::addressof(guard.node->next), node);
		memory::construct_at(std::addressof(guard.node->prev), node->prev);
		
		++_data.size;
		const _NodePointer newNode = guard.release();
		node->prev->next	= newNode;
		node->prev			= newNode;
		return newNode;
	}

	template<class Iter, class Sent>
	void _assign(Iter first, const Sent last) {
		// Assign range [first, last)
		const _NodePointer lastNode = _data.head;
		for (_NodePointer currNode = lastNode->next;;) {
			if (currNode == lastNode) {
				_ListInsertGuard<_MyVal> guard(_data);
				guard.append_range(first, last);
				guard.attach_at_end();
				return;
			}

			if (first == last) {
				this->_erase(currNode, lastNode);
				return;
			}

			currNode->value = *first;
			currNode = currNode->next;
			++first;
		}
	}

	_NodePointer _erase(_NodePointer node) noexcept {
		// Erase node
		const _NodePointer result = node->next;
		node->prev->next = node->next;
		node->next->prev = node->prev;

		_NodeType::free_node(node);
		--_data.size;
		return result;
	}

	iterator _erase(_NodePointer first, _NodePointer last) noexcept {
		// Erase range [first, last)
		if (first != last) {
			const _NodePointer prevNode = first->prev;
			prevNode->next = last;
			last->prev = prevNode;

			do {
				const _NodePointer nextNode = first->next;
				_NodeType::free_node(first);
				--_data.size;

				first = nextNode;
			}
			while (first != last);
		}
		return iterator(last);
	}

	_NodePointer _splice(_NodePointer node, List& other, _NodePointer first, _NodePointer last, const size_type count) {
		// Splice range [first, last) of other at node
		if (this != std::addressof(other)) {
			if (this->max_size() - _data.size < count) {
				this->_length_error();
			}

			_data.size += count;
			other._data.size -= count;
		}

		return this->_splice(node, first, last);
	}

	_NodePointer _splice(_NodePointer node, _NodePointer first, _NodePointer last) {
		// Splice range [first, last) at node
		// Fix up next values
		const _NodePointer firstPrev = first->prev;
		firstPrev->next = last;

		const _NodePointer lastPrev = last->prev;
		lastPrev->next = node;

		const _NodePointer prevNode = node->prev;
		prevNode->next = first;

		// Fix up prev values
		node->prev	= lastPrev;
		last->prev	= firstPrev;
		first->prev = prevNode;

		return last;
	}

	template<class UnaryPred>
	size_type _remove_if(UnaryPred pred, const_iterator first, const_iterator last) {
		// Erase all elements matching pred in range [first, last)
		if (first == last) {
			return 0;
		}

		const auto oldSize = _data.size;
		_ListRemoveGuard<_MyVal> guard(_data);

		const _NodePointer lastNode = last.ptr;
		for (_NodePointer currNode = first.ptr; currNode != lastNode;) {
			if (pred(currNode->value)) {
				currNode = guard.extract(currNode);
			}
			else {
				currNode = currNode->next;
			}
		}
		return oldSize - _data.size;
	}

	template<class BinaryPred>
	size_type _remove_adjacent_if(BinaryPred pred, const_iterator first, const_iterator last) {
		// Erase all adjacent elements matching pred in range [first, last)
		const _NodePointer lastNode = last.ptr;

		_NodePointer currNode = first.ptr;
		if (currNode == lastNode) {
			return 0;
		}

		const auto oldSize = _data.size;
		_ListRemoveGuard<_MyVal> guard(_data);
		for (_NodePointer nextNode = currNode->next; nextNode != lastNode;) {
			if (static_cast<bool>(pred(currNode->value, nextNode->value))) {
				nextNode = guard.extract(nextNode);
			}
			else {
				currNode = nextNode;
				nextNode = currNode->next;
			}
		}
		return oldSize - _data.size;
	}

	template<class Comp>
	void _merge(List& other, Comp comp) {
		// Merge with other, assuming both lists are sorted and elements are compared using comp
		if (this == std::addressof(other)) {
			return;
		}

		const auto otherSize = other._data.size;
		if (otherSize == 0) {
			return;
		}

		const _NodePointer myHead		= _data.head;
		const _NodePointer otherHead	= other._data.head;
		const _NodePointer midNode		= otherHead->next;
		this->_splice(myHead, other, midNode, otherHead, otherSize);

		if (myHead->next != midNode) {
			this->_inplace_merge(myHead->next, midNode, myHead, comp);
		}
	}

	template<class Comp>
	_NodePointer _inplace_merge(_NodePointer first, _NodePointer mid, const _NodePointer last, Comp comp) {
		// Merge 2 sorted ranges [first, mid) and [mid, last), both are in *this
		_NodePointer newFirst;
		if (static_cast<bool>(comp(mid->value, first->value))) {
			newFirst = mid;
		}
		else {
			newFirst = first;
			do {
				first = first->next;
				if (first == mid) {
					return newFirst;
				}
			}
			while (!static_cast<bool>(comp(mid->value, first->value)));
		}

		while (true) {
			_NodePointer currNode = mid;
			do {
				mid = mid->next;
			}
			while (mid != last && static_cast<bool>(comp(mid->value, first->value)));
			
			this->_splice(first, currNode, mid);
			if (mid == last) {
				return newFirst;
			}

			do {
				first = first->next;
				if (first == mid) {
					return newFirst;
				}
			}
			while (!static_cast<bool>(comp(mid->value, first->value)));
		}
	}

	template<class Comp>
	_NodePointer _sort(_NodePointer& first, const size_type length, Comp comp) {
		// Sort range [first, first + length), return first + length
		if (length == 0) {
			return first;
		}

		if (length == 1) {
			return first->next;
		}

		const auto halfLength = length / 2;

		_NodePointer midNode		= this->_sort(first, halfLength, comp);
		const _NodePointer lastNode = this->_sort(midNode, length - halfLength, comp);
		
		first = this->_inplace_merge(first, midNode, lastNode, comp);
		return lastNode;
	}

	[[noreturn]] static void _length_error() {
		throw std::length_error("Max size exceeded!");
	}

private:
	_MyVal _data;
};

template<class T>
void swap(List<T>& lhs, List<T>& rhs) noexcept {
	lhs.swap(rhs);
}

template<class T>
[[nodiscard]] bool operator==(const List<T>& lhs, const List<T>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const List<T>& lhs, const List<T>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // LIST_HPP