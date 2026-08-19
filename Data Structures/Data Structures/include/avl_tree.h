#pragma once
#ifndef ALV_TREE_H
#define ALV_TREE_H

#include<iostream>
#include<queue>

#include"concepts.hpp"
#include"memory.hpp"
#include"node_handle.h"

template<class AVLTreeVal>
class _AVLTreeConstIterator {
private:
	using _NodePointer = typename AVLTreeVal::node_pointer;

public:
	using iterator_concept	= std::bidirectional_iterator_tag;
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename AVLTreeVal::value_type;
	using difference_type	= typename AVLTreeVal::difference_type;
	using pointer			= typename AVLTreeVal::const_pointer;
	using reference			= const value_type&;

	_AVLTreeConstIterator() noexcept
		: ptr() {
	}

	_AVLTreeConstIterator(_NodePointer ptr) noexcept
		: ptr(ptr) {
	}

	[[nodiscard]] reference operator*() const noexcept {
		return ptr->value; // UB: nullptr or end() dereference
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this)); // UB: nullptr or end() dereference
	}

	_AVLTreeConstIterator& operator++() noexcept {
		if (ptr->right->isHead) { // Climb upwards, look for right subtree
			for (_NodePointer currNode = ptr->parent;;) {
				// Stop when reaching head or going upwards from a left subtree for the first time
				if (currNode->isHead || ptr != currNode->right) {
					ptr = currNode; // Goes from the rightmost node to head for end()
					break;
				}
				ptr = std::exchange(currNode, currNode->parent);
			}
		}
		else { // Goes to the leftmost node of right subtree
			ptr = AVLTreeVal::min(ptr->right);
		}
		return *this;
	}

	_AVLTreeConstIterator operator++(int) noexcept {
		_AVLTreeConstIterator temp = *this;
		++(*this);
		return temp;
	}

	_AVLTreeConstIterator& operator--() noexcept {
		if (ptr->isHead) { // Goes back from end() to the rightmost node
			ptr = ptr->right;
		}
		else if (ptr->left->isHead) {
			for (_NodePointer currNode = ptr->parent;;) {
				// Stop when reaching head or going upwards from a right subtree for the first time
				if (currNode->isHead || ptr != currNode->left) {
					if (!ptr->isHead) {
						ptr = currNode;
					}
					break;
				}
				ptr = std::exchange(currNode, currNode->parent);
			}
		}
		else { // Goes to the rightmost node of left subtree
			ptr = AVLTreeVal::max(ptr->left);
		}
		return *this;
	}

	_AVLTreeConstIterator operator--(int) noexcept {
		_AVLTreeConstIterator temp = *this;
		--(*this);
		return temp;
	}

	[[nodiscard]] bool operator==(const _AVLTreeConstIterator& other) const noexcept {
		return ptr == other.ptr;
	}

	[[nodiscard]] bool operator!=(const _AVLTreeConstIterator& other) const noexcept {
		return !(*this == other);
	}

public:
	_NodePointer ptr;
};

template<class AVLTreeVal>
class _AVLTreeIterator : public _AVLTreeConstIterator<AVLTreeVal> {
private:
	using _BaseIter = _AVLTreeConstIterator<AVLTreeVal>;
	using _BaseIter::_BaseIter;

public:
	using iterator_concept	= std::bidirectional_iterator_tag;
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename AVLTreeVal::value_type;
	using difference_type	= typename AVLTreeVal::difference_type;
	using pointer			= typename AVLTreeVal::pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*()); // UB: nullptr or end() dereference
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this)); // UB: nullptr or end() dereference
	}

	_AVLTreeIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	_AVLTreeIterator operator++(int) noexcept {
		_AVLTreeIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	_AVLTreeIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	_AVLTreeIterator operator--(int) noexcept {
		_AVLTreeIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}
};

template<class ValueT, class HeightT, class BalanceT>
struct _AVLTreeNode {
	using node_pointer	= _AVLTreeNode*;
	using value_type	= ValueT;
	using height_type	= HeightT;
	using balance_type	= BalanceT;

	_AVLTreeNode() = default;

	_AVLTreeNode(const _AVLTreeNode&)				= delete;
	_AVLTreeNode& operator=(const _AVLTreeNode&)	= delete;

	[[nodiscard]] static node_pointer construct_head() {
		// Construct empty head sentinel with no value
		const auto newHead = static_cast<node_pointer>(memory::allocate(1, sizeof(_AVLTreeNode)));
		memory::construct_at(std::addressof(newHead->left), newHead);
		memory::construct_at(std::addressof(newHead->right), newHead);
		memory::construct_at(std::addressof(newHead->parent), newHead);
		newHead->height = 0;
		newHead->isHead = true;
		return newHead;
	}

	template<class... Args>
	[[nodiscard]] static node_pointer construct_node(node_pointer head, Args&&... args) {
		// Construct node with value from args
		memory::_NodeAllocateGuard<_AVLTreeNode> guard;
		guard.allocate();
		memory::construct_at(std::addressof(guard.node->value), std::forward<Args>(args)...);
		memory::construct_at(std::addressof(guard.node->left), head);
		memory::construct_at(std::addressof(guard.node->right), head);
		memory::construct_at(std::addressof(guard.node->parent), head);
		guard.node->height = 1;
		guard.node->isHead = false;
		return guard.release();
	}

	static void free_empty_node(node_pointer node) noexcept {
		// Destroy pointer members and deallocate node memory. Only empty nodes should be passed (after destroying value members in free_node() or head node)
		memory::destruct_at(std::addressof(node->left));
		memory::destruct_at(std::addressof(node->right));
		memory::destruct_at(std::addressof(node->parent));
		memory::deallocate(node, sizeof(_AVLTreeNode));
	}

	static void free_node(node_pointer node) noexcept {
		// Destroy entire node, along with its value
		memory::destruct_at(std::addressof(node->value));
		free_empty_node(node);
	}

	void replace_child(node_pointer oldChild, node_pointer newChild) noexcept {
		// If oldChild and *this are parent and child, replace oldChild with newChild
		if (isHead) {
			parent = newChild;
		}
		else if (oldChild == left) {
			left = newChild;
		}
		else if (oldChild == right) {
			right = newChild;
		}
		else {
			return;
		}

		if (newChild) {
			newChild->parent = oldChild->parent;
		}
	}
	// Align for minimal padding
	node_pointer left;		// 8 bytes pointer
	node_pointer right;		// 8 bytes pointer
	node_pointer parent;	// 8 bytes pointer

	value_type	value;		// sizeof(value_type)
	height_type	height;		// 1 byte, assuming AVL tree height <= 255

	bool isHead; // 1 byte boolean
};

enum _NodeChild : bool {
	LEFT, RIGHT
};

template<class NodePtr>
struct _NodeLocation {
	NodePtr		parent;	// Parent node under which new node will be inserted
	_NodeChild	child;	// Whether to insert as left or right child
};

template<class NodePtr>
struct _NodeFindResult {
	_NodeLocation<NodePtr>	location;	// Location to insert new node
	NodePtr					bound;		// Lower bound of the find result, used for duplicate checking
};

template<class NodePtr>
struct _NodeFindHintResult {
	_NodeLocation<NodePtr>	location; // Location to insert new node
	bool					isDuplicate;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class _AVLTreeValue {
public:
	using node_type			= NodeT;
	using node_pointer		= typename NodeT::node_pointer;
	using height_type		= typename NodeT::height_type;
	using balance_type		= typename NodeT::balance_type;

	using value_type		= ValueT;
	using size_type			= SizeT;
	using difference_type	= DiffT;
	using pointer			= Ptr;
	using const_pointer		= ConstPtr;

	_AVLTreeValue() noexcept
		: head(), size(0) {}

	[[nodiscard]] static node_pointer min(node_pointer node)  noexcept {
		// Get the leftmost node in subtree at node
		while (!node->left->isHead) {
			node = node->left;
		}
		return node;
	}

	[[nodiscard]] static node_pointer max(node_pointer node)  noexcept {
		// Get the rightmost node in subtree at node
		while (!node->right->isHead) {
			node = node->right;
		}
		return node;
	}

	[[nodiscard]] static height_type get_height(node_pointer node) noexcept {
		// Get node height
		return static_cast<height_type>(node->height);
	}

	[[nodiscard]] static balance_type get_balance_factor(node_pointer node) noexcept {
		// Get balance factor at node
		if (!node->isHead) {
			const auto leftHeight	= _AVLTreeValue::get_height(node->left);
			const auto rightHeight	= _AVLTreeValue::get_height(node->right);
			return static_cast<balance_type>(rightHeight - leftHeight);
		}
		return 0;
	}

	static void update_height(node_pointer node) noexcept {
		// Update node height
		const auto leftHeight	= _AVLTreeValue::get_height(node->left);
		const auto rightHeight	= _AVLTreeValue::get_height(node->right);
		node->height = static_cast<height_type>(std::max(leftHeight, rightHeight) + 1);
	}

	void rotate_left(node_pointer oldRoot) noexcept {
		// Perform counter-clockwise rotation on subtree at oldRoot
		const node_pointer parent	= oldRoot->parent;
		const node_pointer newRoot	= oldRoot->right;
		const node_pointer child	= newRoot->left;

		parent->replace_child(oldRoot, newRoot);

		oldRoot->parent = newRoot;
		oldRoot->right	= child;
		newRoot->left	= oldRoot;

		if (!child->isHead) { // Reattach newRoot's left child to oldRoot
			child->parent = oldRoot;
		}

		_AVLTreeValue::update_height(oldRoot);
		_AVLTreeValue::update_height(newRoot);
	}

	void rotate_right(node_pointer oldRoot) noexcept {
		// Perform clockwise rotation on subtree at oldRoot
		const node_pointer parent	= oldRoot->parent;
		const node_pointer newRoot	= oldRoot->left;
		const node_pointer child	= newRoot->right;

		parent->replace_child(oldRoot, newRoot);

		oldRoot->parent = newRoot;
		oldRoot->left	= child;
		newRoot->right	= oldRoot;

		if (!child->isHead) { // Reattach newRoot's right child to oldRoot
			child->parent = oldRoot;
		}

		_AVLTreeValue::update_height(oldRoot);
		_AVLTreeValue::update_height(newRoot);
	}

	bool try_rebalance(node_pointer node) noexcept {
		// Check for imbalance and rotate if needed
		const auto nodeBalance = _AVLTreeValue::get_balance_factor(node);
		if (nodeBalance < -1) { // Subtree at node is imbalance to the left
			const auto leftBalance = _AVLTreeValue::get_balance_factor(node->left);
			if (leftBalance <= 0) { // Left - Left
				this->rotate_right(node);
				return true;
			}
			else { // Left - Right
				this->rotate_left(node->left);
				this->rotate_right(node);
				return true;
			}
		}

		if (nodeBalance > 1) { // Subtree at node is imbalance to the right
			const auto rightBalance = _AVLTreeValue::get_balance_factor(node->right);
			if (rightBalance >= 0) { // Right - Right
				this->rotate_left(node);
				return true;
			}
			else { // Right - Left
				this->rotate_right(node->right);
				this->rotate_left(node);
				return true;
			}
		}
		return false;
	}

	void fix_tree(node_pointer node, node_pointer newNode) noexcept {
		// Travel upwards from node to root, update node height and rebalance if needed
		_AVLTreeValue::update_height(newNode); // Reset node height for correct rebalancing

		for (;;) {
			if (node->isHead) { // Reach head before rebalancing
				return;
			}

			_AVLTreeValue::update_height(node);
			if (this->try_rebalance(node)) { // Rebalance, stop trying
				break;
			}
			node = node->parent;
		}

		for (;;) { // Update the remaining nodes height
			node = node->parent;
			if (node->isHead) {
				return;
			}

			_AVLTreeValue::update_height(node);
		}
	}

	node_pointer insert(const _NodeLocation<node_pointer> location, node_pointer newNode) noexcept {
		// Insert newNode at location
		++size;
		if (location.parent == head) { // First node in tree
			newNode->parent = head;
			head->left		= newNode;
			head->right		= newNode;
			head->parent	= newNode;
			return newNode;
		}

		newNode->parent = location.parent;
		if (location.child == _NodeChild::LEFT) { // Insert as left child
			location.parent->left = newNode;
			if (location.parent == head->left) { // New min node, update head->left
				head->left = newNode;
			}
		}
		else { // Insert as right child
			location.parent->right = newNode;
			if (location.parent == head->right) { // New max node, update head->right
				head->right = newNode;
			}
		}

		this->fix_tree(location.parent, newNode);
		return newNode;
	}

	node_pointer extract(_AVLTreeConstIterator<_AVLTreeValue> pos) noexcept {
		// Extract node pointed by pos
		--size;
		const node_pointer extracted	= pos.ptr; // UB: pos == _AVLTree::end()
		if (size == 0) { // Extract final node
			head->left	= head;
			head->right = head;
		}
		else if (extracted == head->left) { // Extract leftmost node
			head->left = (++_AVLTreeConstIterator(pos)).ptr;
		}
		else if (extracted == head->right) { // Extract rightmost node
			head->right = (--_AVLTreeConstIterator(pos)).ptr;
		}

		node_pointer parent = extracted->parent;
		if (!(extracted->left->isHead || extracted->right->isHead)) { // Node has both children
			const node_pointer successor = this->min(extracted->right);
			successor->parent->replace_child(successor, head);
			extracted->parent->replace_child(extracted, successor);

			if (!extracted->left->isHead) { // Adopt extracted's left child
				extracted->left->parent = successor;
				successor->left = std::exchange(extracted->left, head);
			}

			if (!extracted->right->isHead) { // Adopt extracted's right child
				extracted->right->parent = successor;
				successor->right = std::exchange(extracted->right, head);
			}
			extracted->parent = successor; // Fix tree starting point
		}
		else if (extracted->left->isHead && extracted->right->isHead) { // Extract leaf node
			parent->replace_child(extracted, head);
		}
		else { // Node has a single child
			const node_pointer childNode = std::exchange(
				(extracted->left->isHead) ? extracted->right : extracted->left, head
			);
			parent->replace_child(extracted, childNode);
		}

		this->fix_tree(std::exchange(extracted->parent, head), extracted);
		return extracted;
	}

	void clear_subtree(node_pointer node) noexcept {
		// Clear entire subtree at node recursively
		while (!node->isHead) {
			this->clear_subtree(node->right);
			node_type::free_node(std::exchange(node, node->left));
		}
	}

	void clear() noexcept {
		this->clear_subtree(head->parent);
		node_type::free_empty_node(head);
	}

	void swap(_AVLTreeValue& other) noexcept {
		using std::swap;
		swap(head, other.head);
		swap(size, other.size);
	}

	/*
		Node head serves as the root sentinel and end() node for tree traversal

		head->left:		points to the leftmost node (min node)
		head->right:	points to the rightmost node (max node)
		head->parent:	points to the actual root node
	*/
	node_pointer	head;
	size_type		size;
};

template<class NodeT>
struct _AVLTempNodeGuard {
	using node_type		= NodeT;
	using node_pointer	= typename NodeT::node_pointer;

	template<class... Args>
	_AVLTempNodeGuard(node_pointer head, Args&&... args)
		: base() {
		base.allocate();
		memory::construct_at(std::addressof(base.node->value), std::forward<Args>(args)...);
		memory::construct_at(std::addressof(base.node->left), head);
		memory::construct_at(std::addressof(base.node->right), head);
		memory::construct_at(std::addressof(base.node->parent), head);
		base.node->height = 1;
		base.node->isHead = false;
	}

	_AVLTempNodeGuard(const _AVLTempNodeGuard&)				= delete;
	_AVLTempNodeGuard& operator=(const _AVLTempNodeGuard&)	= delete;

	~_AVLTempNodeGuard() {
		if (base.node) {
			memory::destruct_at(std::addressof(base.node->value));
			memory::destruct_at(std::addressof(base.node->left));
			memory::destruct_at(std::addressof(base.node->right));
			memory::destruct_at(std::addressof(base.node->parent));
		}
	}

	node_pointer release() noexcept {
		return base.release();
	}

	decltype(auto) get_value() noexcept {
		return base.node->value;
	}

	memory::_NodeAllocateGuard<node_type> base;
};

template<class AVLTreeVal>
struct _TreeConstructGuard {
	using node_type = typename AVLTreeVal::node_type;

	_TreeConstructGuard(AVLTreeVal& data)
		: data(std::addressof(data)) {
		data->head = node_type::construct_head();
	}

	_TreeConstructGuard(const _TreeConstructGuard&)				= delete;
	_TreeConstructGuard& operator=(const _TreeConstructGuard&)	= delete;

	~_TreeConstructGuard() noexcept {
		if (data) {
			data->clear();
		}
	}

	void release() noexcept {
		data = nullptr;
	}

	AVLTreeVal* data;
};

template<class AVLTreeVal>
struct _SubtreeCopyGuard {
	using node_pointer = typename AVLTreeVal::node_pointer;

	_SubtreeCopyGuard(AVLTreeVal& data, node_pointer newRoot)
		: data(std::addressof(data)), newRoot(newRoot) {}

	_SubtreeCopyGuard(const _SubtreeCopyGuard&)				= delete;
	_SubtreeCopyGuard& operator=(const _SubtreeCopyGuard&)	= delete;

	~_SubtreeCopyGuard() noexcept {
		if (data) {
			data->clear_subtree(newRoot);
		}
	}

	void release() noexcept {
		data = nullptr;
	}

	AVLTreeVal*		data;
	node_pointer	newRoot;
};

template<class Traits>
class _AVLTree {
public:
	using key_type		= typename Traits::key_type;
	using key_compare	= typename Traits::key_compare;
	using value_compare = typename Traits::value_compare;

	using value_type		= typename Traits::value_type;
	using size_type			= std::size_t;
	using difference_type	= std::ptrdiff_t;
	using pointer			= value_type*;
	using const_pointer		= const value_type*;
	using reference			= value_type&;
	using const_reference	= const value_type&;

protected:
	using _NodeType		= _AVLTreeNode<value_type, uint8_t, int8_t>;
	using _NodePointer	= typename _NodeType::node_pointer;

	using _MyVal		= _AVLTreeValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

	static constexpr bool _isMulti	= Traits::isMulti;
	static constexpr bool _isMap	= Traits::isMap;

	enum class _CopyStrategy : bool {
		Copy,
		Move
	};

public:
	using iterator			= _AVLTreeConstIterator<_MyVal>;
	using const_iterator	= _AVLTreeConstIterator<_MyVal>;

	using reverse_iterator			= std::reverse_iterator<iterator>;
	using const_reverse_iterator	= std::reverse_iterator<const_iterator>;

	using node_handle			= typename Traits::node_handle;
	using insert_return_type	= _InsertReturnType<iterator, node_handle>;
	
public:
	_AVLTree()
		: _data() {
		_data.head = _NodeType::construct_head();
	}

	_AVLTree(const _AVLTree& other)
		: _data() {
		_TreeConstructGuard<_MyVal> guard(_data);
		this->_copy<_CopyStrategy::Copy>(other);
		guard.release();
	}
	
	template<std::input_iterator It>
		requires std::sentinel_for<It, It>
	_AVLTree(It first, It last)
		: _data() {
		_TreeConstructGuard<_MyVal> guard(_data);
		this->insert(first, last);
		guard.release();
	}

	_AVLTree(std::initializer_list<value_type> initList)
		: _data() {
		_TreeConstructGuard<_MyVal> guard(_data);
		this->insert(initList);
		guard.release();
	}

	_AVLTree(_AVLTree&& other) noexcept
		: _data() {
		_data.head = _NodeType::construct_head();
		_data.swap(other._data);
	}

	~_AVLTree() noexcept {
		_data.clear();
	}

	_AVLTree& operator=(const _AVLTree& other) {
		if (this != std::addressof(other)) {
			this->clear();
			this->_copy<_CopyStrategy::Copy>(other);
		}
		return *this;
	}

	_AVLTree& operator=(_AVLTree&& other) noexcept {
		if (this != std::addressof(other)) {
			this->clear();
			_data.swap(other._data);
		}
		return *this;
	}

	_AVLTree& operator=(std::initializer_list<value_type> initList) {
		this->clear();
		this->insert(initList);
		return *this;
	}

	[[nodiscard]] iterator begin() noexcept {
		return iterator(_data.head->left);
	}

	[[nodiscard]] const_iterator begin() const noexcept {
		return const_iterator(_data.head->left);
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

	[[nodiscard]] reference min() noexcept {
		return _data.head->left->value; // UB
	}

	[[nodiscard]] const_reference min() const noexcept {
		return _data.head->left->value;
	}

	[[nodiscard]] reference max() noexcept {
		return _data.head->right->value; // UB
	}

	[[nodiscard]] const_reference max() const noexcept {
		return _data.head->right->value;
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

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.size == 0;
	}

	template<class... Args>
	std::pair<iterator, bool> emplace(Args&&... args) {
		// Insert by constructing in place using args
		const auto result = this->_emplace(std::forward<Args>(args)...);
		return { iterator(result.first), result.second };
	}

	template<class... Args>
	iterator emplace_hint(const_iterator hint, Args&&... args) {
		// Insert with hint by constructing in place using args
		return iterator(this->_emplace_hint(hint.ptr, std::forward<Args>(args)...));
	}

	std::pair<iterator, bool> insert(const value_type& val) {
		// Insert by copying val
		return this->emplace(val);
	}

	std::pair<iterator, bool> insert(value_type&& val) {
		// Insert by moving val
		return this->emplace(std::move(val));
	}

	iterator insert(const_iterator hint, const value_type& val) {
		// Insert with hint by copying val
		return this->emplace_hint(hint, val);
	}

	iterator insert(const_iterator hint, value_type&& val) {
		// Insert with hint by moving val
		return this->emplace_hint(hint, std::move(val));
	}

	template<std::input_iterator It>
		requires std::sentinel_for<It, It>
	void insert(It first, It last) {
		// Insert range [first, last)
		for (; first != last; ++first) {
			this->_emplace_hint(_data.head, *first);
		}
	}

	void insert(std::initializer_list<value_type> initList) {
		// Insert initList
		this->insert(initList.begin(), initList.end());
	}

	template<class iterator = iterator>
		requires !std::same_as<It, const_iterator>
	iterator erase(iterator where) noexcept {
		return iterator(this->_erase(where)); // UB
	}

	iterator erase(const_iterator pos) noexcept {
		// Erase at pos
		return iterator(this->_erase(pos));
	}

	iterator erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		return iterator(this->_erase(first, last));
	}

	size_type erase(const key_type& key) noexcept {
		// Erase key
		if constexpr (false) {
			this->_erase(this->_equal_range(key));
		}
		else {
			const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(key);
			if (!this->_is_duplicate_key(result.bound, key)) { // Key does not exist
				return 0;
			}
			this->_erase(const_iterator(result.bound));
			return 1;
		}
	}

	/*
		STL requires C++23 for heterogeneous erase
		Use with caution
	*/
	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	bool erase(const KeyT& key) noexcept {
		// Erase key
		const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(key);
		if (!this->_is_duplicate_key(result.bound, key)) { // Key does not exist
			return false;
		}
		this->_erase(const_iterator(result.bound));
		return true;
	}

	void clear() noexcept {
		// Erase all
		_data.clear_subtree(_data.head->parent);
		_data.head->left	= _data.head;
		_data.head->right	= _data.head;
		_data.head->parent	= _data.head;
		_data.size			= 0;
	}

	void swap(_AVLTree& other) noexcept {
		// Swap contents with other
		if (this != std::addressof(other)) {
			_data.swap(other._data);
		}
	}

	[[nodiscard]] iterator find(const key_type& key) {
		// Find key
		return iterator(this->_find(key));
	}

	[[nodiscard]] const_iterator find(const key_type& key) const {
		// Find key
		return const_iterator(this->_find(key));
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] iterator find(const KeyT& key) {
		// Find element equivalent to key
		return iterator(this->_find(key));
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] const_iterator find(const KeyT& key) const {
		// Find element equivalent to key
		return const_iterator(this->_find(key));
	}

	[[nodiscard]] bool contains(const key_type& key) const {
		// Check if tree contains key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] bool contains(const KeyT& key) const {
		// Check if tree contains element equivalent to key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	[[nodiscard]] size_type count(const key_type& key) const {
		// Count occurrences of key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] size_type count(const KeyT& key) const {
		// Count occurrences of value equivalent to key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	[[nodiscard]] iterator lower_bound(const key_type& key) {
		// Find the first element not less than key
		return iterator(this->_find_lower_bound(key).bound);
	}

	[[nodiscard]] const_iterator lower_bound(const key_type& key) const {
		// Find the first element not less than key
		return const_iterator(this->_find_lower_bound(key).bound);
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] iterator lower_bound(const KeyT& key) {
		// Find the first equivalent element not less than key
		return iterator(this->_find_lower_bound(key).bound);
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] const_iterator lower_bound(const KeyT& key) const {
		// Find the first equivalent element not less than key
		return const_iterator(this->_find_lower_bound(key).bound);
	}

	[[nodiscard]] iterator upper_bound(const key_type& key) {
		// Find the first element greater than key
		return iterator(this->_find_upper_bound(key).bound);
	}

	[[nodiscard]] const_iterator upper_bound(const key_type& key) const {
		// Find the first element greater than key
		return const_iterator(this->_find_upper_bound(key).bound);
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] iterator upper_bound(const KeyT& key) {
		// Find the first equivalent element greater than key
		return iterator(this->_find_upper_bound(key).bound);
	}

	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	[[nodiscard]] const_iterator upper_bound(const KeyT& key) const {
		// Find the first equivalent element greater than key
		return const_iterator(this->_find_upper_bound(key).bound);
	}

	template<class>
	friend class _AVLTree;

	template<class Traits2>
	void merge(_AVLTree<Traits2>& other) {
		// Merge other into *this, leaving other empty
		if constexpr (std::is_same_v<_AVLTree, _AVLTree<Traits2>>) {
			if (this == std::addressof(other)) {
				return;
			}
		}

		for (auto iter = other.begin(); iter != other.end();) {
			const _NodePointer currNode = iter.ptr;
			++iter; // Important: increment iterator before extraction

			const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(currNode->value);
			if (this->_is_duplicate_key(result.bound, currNode->value)) {
				continue;
			}

			if (this->max_size() == _data.size) {
				this->_length_error();
			}
			// Extract from other and reset links
			const _NodePointer extracted	= other._data.extract(const_iterator(currNode));
			extracted->left					= _data.head;
			extracted->right				= _data.head;
			// Insert back into *this
			_data.insert(result.location, extracted); // Handle extracted->parent and extracted->height
		}
	}

	template<class Traits2>
	void merge(_AVLTree<Traits2>&& other) {
		// Merge other into *this, leaving other empty
		this->merge(other);
	}

	node_handle extract(const_iterator pos) {
		// Extract node at pos, return its node_handle
		return node_handle::make(_data.extract(pos));
	}

	node_handle extract(const key_type& key) {
		// Extract node with key, return its node_handle
		const auto pos = this->find(key);
		if (pos == this->end()) {
			return node_handle{};
		}
		return this->extract(pos);
	}

	auto insert(node_handle&& handle) {
		// Insert node from handle
		if (handle.is_empty()) {
			return insert_return_type{ this->end(), false, node_handle{} };
		}

		const auto node = handle.get_pointer();
		const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(node->value);
		if (this->_is_duplicate_key(result.bound, node->value)) {
			return insert_return_type{ iterator(result.bound), false, std::move(handle) };
		}

		if (this->max_size() == _data.size) {
			this->_length_error();
		}

		node->left	= _data.head;
		node->right = _data.head;
		const auto inserted = _data.insert(result.location, handle._release());
		return insert_return_type{ iterator(inserted), true, std::move(handle) };
	}

	iterator insert(const const_iterator hint, node_handle&& handle) {
		// Insert node from handle with hint
		if (handle.is_empty()) {
			return this->end();
		}

		const auto node = handle.get_pointer();
		const _NodeFindHintResult<_NodePointer> result = this->_find_hint(hint.ptr, node->value);
		if (result.isDuplicate) {
			return iterator(result.location.parent);
		}

		if (this->max_size() == _data.size) {
			this->_length_error();
		}

		node->left	= _data.head;
		node->right = _data.head;
		const auto inserted = _data.insert(result.location, handle._release());
		return iterator(inserted);
	}

	void level_order() {
		// Print subtree at node in level-order
		_NodePointer root = _data.head->parent;
		if (root->isHead) {
			return;
		}

		_NodePointer bound = root;

		std::queue<_NodePointer> nodesQueue;
		nodesQueue.push(root);
		while (!nodesQueue.empty()) {
			const _NodePointer node = nodesQueue.front();
			std::cout << node->value << " ";
			
			const bool isBound = node == bound;
			if (isBound) {
				std::cout << "| ";
			}

			nodesQueue.pop();
			if (!node->left->isHead) {
				nodesQueue.push(node->left);
				if (isBound) {
					bound = node->left;
				}
			}
			if (!node->right->isHead) {
				nodesQueue.push(node->right);
				if (isBound) {
					bound = node->right;
				}
			}
		}
		std::cout << "\n";
	}

private:
	template<_CopyStrategy _strat, class T2>
	_NodePointer _copy_node(T2& val) {
		// Construct node by copying or moving val
		if constexpr (_strat == _CopyStrategy::Copy) {
			return _NodeType::construct_node(_data.head, val);
		}
		else {
			return _NodeType::construct_node(_data.head, std::move(val));
		}
	}

	template<_CopyStrategy _strat>
	_NodePointer _copy_subtree(_NodePointer oldRoot, _NodePointer where) {
		// Copy subtree at oldRoot into where recursively
		_NodePointer newRoot = _data.head;
		if (!oldRoot->isHead) {
			newRoot			= this->_copy_node<_strat>(oldRoot->value);
			newRoot->parent = where;
			newRoot->height = oldRoot->height;

			_SubtreeCopyGuard<_MyVal> guard(_data, newRoot);
			newRoot->left	= this->_copy_subtree<_strat>(oldRoot->left, newRoot);
			newRoot->right	= this->_copy_subtree<_strat>(oldRoot->right, newRoot);
			guard.release();
		}
		return newRoot;
	}

	template<_CopyStrategy _strat>
	void _copy(const _AVLTree& other) {
		// Copy entire tree from other
		_data.head->parent	= this->_copy_subtree<_strat>(other._data.head->parent, _data.head);
		_data.size			= other._data.size;
		// Update leftmost and rightmost nodes
		if (_data.head->parent->isHead) { // Empty tree
			_data.head->left	= _data.head;
			_data.head->right	= _data.head;
		}
		else { // Non-empty tree, find min and max
			_data.head->left	= _MyVal::min(_data.head->parent);
			_data.head->right	= _MyVal::max(_data.head->parent);
		}
	}

	template<class KeyT>
	[[nodiscard]] bool _is_duplicate_key(_NodePointer bound, const KeyT& key) const {
		// Check if key is duplicate by comparing with bound
		return !bound->isHead && !(_comp(key, bound->value));
	}

	template<class KeyT>
	[[nodiscard]] _NodeFindResult<_NodePointer> _find_lower_bound(const KeyT& key) const {
		/*
			Find the smallest (or leftmost in-order) node that is not less than key (or does not satisfy _comp(node value, key))

			Traverse the whole path downwards from root until nullptr is reached.
			At each node, perform exactly 01 comparison using _comp::operator() on key and node value.

			Let N be the total number of nodes:
			Best case:		O(1),		using 01 comparison (root case)
			Worst case:		O(log2(N)), using log2(N) comparisons
			Average case:	O(log2(N))
		*/
		_NodeFindResult<_NodePointer> result{ { _data.head->parent, _NodeChild::RIGHT }, _data.head };
		for (_NodePointer currNode = result.location.parent; !currNode->isHead;) {
			result.location.parent = currNode;
			if (_comp(currNode->value, key)) {
				result.location.child	= _NodeChild::RIGHT;
				currNode				= currNode->right;
			}
			else {
				result.location.child	= _NodeChild::LEFT;
				result.bound			= currNode;
				currNode				= currNode->left;
			}
		}
		return result;
	}

	template<class KeyT>
	[[nodiscard]] _NodeFindResult<_NodePointer> _find_upper_bound(const KeyT& key) const {
		// Find the smallest (or leftmost in-order) node that is strictly greater than key (or satisfies _comp(key, node value))
		_NodeFindResult<_NodePointer> result{ { _data.head->parent, _NodeChild::RIGHT }, _data.head };
		for (_NodePointer currNode = result.location.parent; !currNode->isHead;) {
			result.location.parent = currNode;
			if (_comp(key, currNode->value)) {
				result.location.child	= _NodeChild::LEFT;
				result.bound			= currNode;
				currNode				= currNode->left;
			}
			else {
				result.location.child	= _NodeChild::RIGHT;
				currNode				= currNode->right;
			}
		}
		return result;
	}

	template<class KeyT>
	[[nodiscard]] _NodeFindHintResult<_NodePointer> _find_hint(_NodePointer hintNode, const KeyT& key) const {
		// Find node insert location using hintNode
		const _NodePointer head = _data.head;
		if (hintNode == head->left) { // Insert at begin as leftmost node
			if (_comp(key, hintNode->value)) {
				return { { hintNode, _NodeChild::LEFT }, false };
			}
		}
		else if (hintNode->isHead) { // Insert at end as rightmost node
			if (head->parent->isHead || _comp(head->right->value, key)) {
				return { { head->right, _NodeChild::RIGHT }, false };
			}
		}
		else if (_comp(key, hintNode->value)) { // key < *hintNode
			const _NodePointer prevNode = (--const_iterator(hintNode)).ptr;
			if (_comp(prevNode->value, key)) { // *(--hintNode) < key < *hintNode, insert here
				if (prevNode->right->isHead) {
					return { { prevNode, _NodeChild::RIGHT }, false };
				}
				else {
					return { { hintNode, _NodeChild::LEFT }, false };
				}
			}
		}
		else if (_comp(hintNode->value, key)) { // key > *hintNode
			const _NodePointer nextNode = (++const_iterator(hintNode)).ptr;
			if (nextNode->isHead || _comp(key, nextNode->value)) { // *hintNode < key < *(++hintNode), insert here
				if (hintNode->right->isHead) {
					return { { hintNode, _NodeChild::RIGHT }, false };
				}
				else {
					return { { nextNode, _NodeChild::LEFT }, false };
				}
			}
		}
		else { // Duplicate value, don't insert
			return { { hintNode, _NodeChild::LEFT, }, true };
		}
		// Incorrect hint, key is not in the proximity of *hintNode. Resort to the usual find method
		const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(key);
		if (this->_is_duplicate_key(result.bound, key)) {
			return { { result.bound, _NodeChild::LEFT }, true };
		}
		return { result.location, false };
	}

	template<class... Args>
	std::pair<_NodePointer, bool> _emplace(Args&&... args) {
		// Insert by constructing node inplace using args
		_AVLTempNodeGuard<_NodeType> guard(_data.head, std::forward<Args>(args)...); // Create temporary node for initial node search
		const auto& key = guard.get_value();

		const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(key); // Find insert location
		if (this->_is_duplicate_key(result.bound, key)) { // Duplicate check
			return { result.bound, false };
		}

		if (this->max_size() == _data.size) {
			this->_length_error();
		}

		const _NodePointer newNode = guard.release(); // Safe to insert, release temp node, transfer ownership to *this
		return { _data.insert(result.location, newNode), true };
	}

	template<class... Args>
	_NodePointer _emplace_hint(_NodePointer hintNode, Args&&... args) {
		// Insert by constructing node inplace using args with given hint
		_AVLTempNodeGuard<_NodeType> guard(_data.head, std::forward<Args>(args)...);
		const auto& key = guard.get_value();

		const _NodeFindHintResult<_NodePointer> result = this->_find_hint(hintNode, key);
		if (result.isDuplicate) {
			return result.location.parent;
		}

		if (this->max_size() == _data.size) {
			this->_length_error();
		}

		const _NodePointer newNode = guard.release();
		return _data.insert(result.location, newNode);
	}

	template<class KeyT>
	std::pair<_NodePointer, _NodePointer> _equal_range(const KeyT& key) const {
		// Find the range of nodes equivalent to key
		_NodePointer currNode	= _data.head->parent;
		_NodePointer lowNode	= _data.head; // end() if search fails
		_NodePointer highNode	= _data.head; // end() if search fails

		while (!currNode->isHead) {
			if (_comp(currNode->value, key)) {
				currNode = currNode->right; // Descend right subtree
			}
			else { // currNode is not less than key, remember it
				if (highNode->isHead && _comp(key, currNode->value)) {
					highNode = currNode; // currNode is greater than key, remember it
				}

				lowNode		= currNode;
				currNode	= currNode->left; // Descend left subtree
			}
		}

		currNode = highNode->isHead ? _data.head->parent : highNode->left; // Continue searching for upper bound
		while (!currNode->isHead) {
			if (_comp(key, currNode->value)) { // currNode is greater than key, remember it
				highNode = currNode;
				currNode = currNode->left; // Descend left subtree
			}
			else {
				currNode = currNode->right; // Descend right subtree
			}
		}
		return { lowNode, highNode };
	}

	_NodePointer _erase(const_iterator pos) noexcept {
		// Erase node at pos, return the next in-order node
		const auto next = (++const_iterator(pos));
		_NodeType::free_node(_data.extract(pos)); // UB
		return next.ptr;
	}

	_NodePointer _erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		const auto begin = this->begin();
		if (first == this->begin() && last == this->end()) { // Erase all elements
			this->clear();
			return last.ptr;
		}
		// Erase nodes one at a time
		while (first != last) {
			this->_erase(first++); // UB
		}
		return last.ptr;
	}

	size_type _erase(const std::pair<_NodePointer, _NodePointer> where) noexcept {
		const const_iterator first(where.first);
		const const_iterator last(where.second);
		const auto count = static_cast<size_type>(std::distance(first, last));
		this->_erase(first, last);
		return count;
	}

	template<class KeyT>
	[[nodiscard]] _NodePointer _find(const KeyT& key) const {
		// Find element equivalent to key
		const _NodeFindResult<_NodePointer> result = this->_find_lower_bound(key);
		if (this->_is_duplicate_key(result.bound, key)) {
			return result.bound;
		}
		return _data.head;
	}

	[[noreturn]] static void _length_error() {
		throw std::length_error("Max size exceeded!");
	}

private:
	_MyVal		_data;
	key_compare	_comp;
};

template<
	class KeyT,
	class T,
	class Comp,
	bool _isMulti
>
class _TreeTraits {
public:
	using key_type		= KeyT;
	using value_type	= T;
	using key_compare	= Comp;
	using value_compare = key_compare;

	using node_handle = _NodeHandle<
		_AVLTreeNode<value_type, uint8_t, int8_t>, _NodeHandleBase, key_type
	>;

	static constexpr bool isMulti	= _isMulti;
	static constexpr bool isMap		= false;

	static const key_type& key_from_node(const value_type& val) {
		return val;
	}
};

template<class T, class Comp = std::less<>>
using AVLTree = _AVLTree<_TreeTraits<T, T, Comp, false>>;

template<class T, class Comp = std::less<>>
using AVLMultiTree = _AVLTree<_TreeTraits<T, T, Comp, true>>;
#endif // ALV_TREE_H