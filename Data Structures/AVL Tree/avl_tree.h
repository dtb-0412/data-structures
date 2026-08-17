#pragma once
#ifndef ALV_TREE_H
#define ALV_TREE_H

#include<iostream>

#include"concepts.hpp"
#include"memory.hpp"
#include"node_handle.h"

//enum TreeOrder {
//	PRE_ORDER, IN_ORDER, POST_ORDER, LEVEL_ORDER
//};

template<class AVLTreeVal>
class AVLTreeConstIterator {
private:
	using _NodePointer = typename AVLTreeVal::NodePointer;

public:
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename AVLTreeVal::ValueType;
	using difference_type	= typename AVLTreeVal::DifferenceType;
	using pointer			= typename AVLTreeVal::ConstPointer;
	using reference			= const value_type&;

	AVLTreeConstIterator() noexcept
		: ptr() {
	}

	AVLTreeConstIterator(_NodePointer ptr) noexcept
		: ptr(ptr) {
	}

	[[nodiscard]] reference operator*() const noexcept {
		return ptr->value; // UB: nullptr or end() dereference
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this)); // UB: nullptr or end() dereference
	}

	AVLTreeConstIterator& operator++() noexcept {
		if (!ptr->right) { // Climb upwards, look for right subtree
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

	AVLTreeConstIterator& operator++(int) noexcept {
		AVLTreeConstIterator temp = *this;
		++(*this);
		return temp;
	}

	AVLTreeConstIterator& operator--() noexcept {
		if (ptr->isHead) { // Goes back from end() to the rightmost node
			ptr = ptr->right;
		}
		else if (!ptr->left) {
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

	AVLTreeConstIterator& operator--(int) noexcept {
		AVLTreeConstIterator temp = *this;
		--(*this);
		return temp;
	}

	[[nodiscard]] bool operator==(const AVLTreeConstIterator& other) const noexcept {
		return ptr == other.ptr;
	}

	[[nodiscard]] bool operator!=(const AVLTreeConstIterator& other) const noexcept {
		return !(*this == other);
	}

public:
	_NodePointer ptr;
};

template<class AVLTreeVal>
class AVLTreeIterator : public AVLTreeConstIterator<AVLTreeVal> {
private:
	using _BaseIter = AVLTreeConstIterator<AVLTreeVal>;
	using _BaseIter::_BaseIter;

public:
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename AVLTreeVal::ValueType;
	using difference_type	= typename AVLTreeVal::DifferenceType;
	using pointer			= typename AVLTreeVal::Pointer;
	using reference			= value_type&;

	[[nodiscard]] reference operator*() const noexcept {
		return const_cast<reference>(_BaseIter::operator*()); // UB: nullptr or end() dereference
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this)); // UB: nullptr or end() dereference
	}

	AVLTreeIterator& operator++() noexcept {
		_BaseIter::operator++();
		return *this;
	}

	AVLTreeIterator& operator++(int) noexcept {
		AVLTreeIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	AVLTreeIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	AVLTreeIterator& operator--(int) noexcept {
		AVLTreeIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}
};

template<class ValueT, class HeightT, class BalanceT>
struct AVLTreeNode {
	using NodePointer	= AVLTreeNode*;
	using ValueType		= ValueT;
	using HeightType	= HeightT;
	using BalanceType	= BalanceT;

	AVLTreeNode() = default;

	AVLTreeNode(const AVLTreeNode&) = delete;
	AVLTreeNode& operator=(const AVLTreeNode&) = delete;

	[[nodiscard]] static NodePointer construct_head() {
		// Construct empty head node, no value
		const NodePointer newHead = static_cast<NodePointer>(memory::allocate(1, sizeof(AVLTreeNode)));
		memory::construct_at(std::addressof(newHead->left), newHead);
		memory::construct_at(std::addressof(newHead->right), newHead);
		newHead->parent = nullptr;
		newHead->isHead = true;
		return newHead;
	}

	template<class... Args>
	[[nodiscard]] static NodePointer construct_node(Args&&... args) {
		// Construct node from args
		const NodePointer newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(AVLTreeNode)));
		memory::construct_at(std::addressof(newNode->value), std::forward<Args>(args)...);
		newNode->left = nullptr;
		newNode->right = nullptr;
		newNode->parent = nullptr;
		newNode->height = 1;
		newNode->isHead = false;
		return newNode;
	}

	static void free_empty_node(NodePointer node) noexcept {
		// Destroy pointer members and deallocate node memory. Only empty nodes should be passed (after destroying value members in free_node() or head node)
		memory::destruct_at(std::addressof(node->left));
		memory::destruct_at(std::addressof(node->right));
		memory::destruct_at(std::addressof(node->parent));
		memory::deallocate(node, sizeof(AVLTreeNode));
	}

	static void free_node(NodePointer node) noexcept {
		// Destroy entire node, along with its value
		memory::destruct_at(std::addressof(node->value));
		free_empty_node(node);
	}

	void release_child(NodePointer child) noexcept {
		// Release child, unlink it from *this
		this->replace_child(child, nullptr);
	}

	void replace_child(NodePointer oldChild, NodePointer newChild) noexcept {
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
	NodePointer left;	// 8 bytes pointer
	NodePointer right;	// 8 bytes pointer
	NodePointer parent;	// 8 bytes pointer

	ValueType	value;	// sizeof(ValueType)
	HeightType	height; // 1 byte, assuming AVL tree height <= 255

	bool isHead;		// 1 byte boolean
};

template<class NodeT>
struct AVLTreeTempNode {
	// Struct to temporarily store a constructed node
	using NodeType = NodeT;
	using NodePointer = typename NodeType::NodePointer;
	using ValueType = typename NodeType::ValueType;


	template<class... Args>
	explicit AVLTreeTempNode(Args&&... args)
		: ptr(nullptr) { // Prevent double delete when allocation throws
		ptr = NodeType::construct_node(std::forward<Args>(args)...);
	}

	AVLTreeTempNode(const AVLTreeTempNode&) = delete;
	AVLTreeTempNode& operator=(const AVLTreeTempNode&) = delete;

	~AVLTreeTempNode() noexcept {
		if (ptr) {
			NodeType::free_node(this->release());
		}
	}

	[[nodiscard]] NodePointer release() noexcept {
		// Give up node ownership and return contained pointer
		return std::exchange(ptr, nullptr);
	}

	[[nodiscard]] const ValueType& get_value() noexcept {
		return ptr->value;
	}

	NodePointer ptr;
};

enum NodeChild : bool {
	LEFT, RIGHT
};

template<class NodePtr>
struct NodeLocation {
	NodePtr parent;		// Parent node under which new node will be inserted
	NodeChild child;	// Whether to insert as left or right child
};

template<class NodePtr>
struct NodeFindResult {
	NodePtr bound;					// Lower bound of the find result, used for duplicate checking
	NodeLocation<NodePtr> location;	// Location to insert new node
};

template<class NodePtr>
struct NodeFindHintResult {
	NodeLocation<NodePtr> location; // Location to insert new node
	bool isDuplicate;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class AVLTreeValue {
public:
	using NodeType = NodeT;
	using NodePointer = typename NodeType::NodePointer;
	using HeightType = typename NodeType::HeightType;
	using BalanceType = typename NodeType::BalanceType;

	using ValueType = ValueT;
	using SizeType = SizeT;
	using DifferenceType = DiffT;
	using Pointer = Ptr;
	using ConstPointer = ConstPtr;
	using Reference = ValueType&;
	using ConstReference = const ValueType&;

	AVLTreeValue() noexcept
		: head(), size(0) {
	}

	[[nodiscard]] static NodePointer min(NodePointer node)  noexcept {
		// Get the leftmost node in subtree at node
		while (node->left) {
			node = node->left;
		}
		return node;
	}

	[[nodiscard]] static NodePointer max(NodePointer node)  noexcept {
		// Get the rightmost node in subtree at node
		while (node->right) {
			node = node->right;
		}
		return node;
	}

	[[nodiscard]] static HeightType get_height(const NodePointer node) noexcept {
		// Get node height
		return static_cast<HeightType>(node ? node->height : 0);
	}

	[[nodiscard]] static BalanceType get_balance_factor(const NodePointer node) noexcept {
		// Get balance factor at node
		if (node) {
			const auto leftHeight = AVLTreeValue::get_height(node->left);
			const auto rightHeight = AVLTreeValue::get_height(node->right);
			return static_cast<BalanceType>(rightHeight - leftHeight);
		}
		return 0;
	}

	static void update_height(const NodePointer node) noexcept {
		// Update node height
		const auto leftHeight = AVLTreeValue::get_height(node->left);
		const auto rightHeight = AVLTreeValue::get_height(node->right);
		node->height = static_cast<HeightType>(std::max(leftHeight, rightHeight) + 1);
	}

	void rotate_left(const NodePointer oldRoot) noexcept {
		// Perform counter-clockwise rotation on subtree at oldRoot
		const NodePointer parent = oldRoot->parent;
		const NodePointer newRoot = oldRoot->right;
		const NodePointer child = newRoot->left;

		parent->replace_child(oldRoot, newRoot);
		oldRoot->parent = newRoot;
		oldRoot->right = child;
		newRoot->left = oldRoot;

		if (child) { // Reattach newRoot's left child to oldRoot
			child->parent = oldRoot;
		}

		AVLTreeValue::update_height(oldRoot);
		AVLTreeValue::update_height(newRoot);
	}

	void rotate_right(const NodePointer oldRoot) noexcept {
		// Perform clockwise rotation on subtree at oldRoot
		const NodePointer parent = oldRoot->parent;
		const NodePointer newRoot = oldRoot->left;
		const NodePointer child = newRoot->right;

		parent->replace_child(oldRoot, newRoot);
		oldRoot->parent = newRoot;
		oldRoot->left = child;
		newRoot->right = oldRoot;

		if (child) { // Reattach newRoot's right child to oldRoot
			child->parent = oldRoot;
		}

		AVLTreeValue::update_height(oldRoot);
		AVLTreeValue::update_height(newRoot);
	}

	bool try_rebalance(const NodePointer node) noexcept {
		// Check for imbalance and rotate if needed
		const auto nodeBalance = AVLTreeValue::get_balance_factor(node);
		if (nodeBalance < -1) { // Subtree at node is imbalance to the left
			const auto leftBalance = AVLTreeValue::get_balance_factor(node->left);
			if (leftBalance <= 0) { // Left - Left
				this->rotate_right(node);
				return true;
			}
			// Left - Right
			this->rotate_left(node->left);
			this->rotate_right(node);
			return true;
		}

		if (nodeBalance > 1) { // Subtree at node is imbalance to the right
			const auto rightBalance = AVLTreeValue::get_balance_factor(node->right);
			if (rightBalance >= 0) { // Right - Right
				this->rotate_left(node);
				return true;
			}
			// Right - Left
			this->rotate_right(node->right);
			this->rotate_left(node);
			return true;
		}
		return false;
	}

	void fixTree(NodePointer node, NodePointer newNode) noexcept {
		// Travel upwards from node to root, update node height and rebalance if needed
		AVLTreeValue::update_height(newNode); // Reset node height for correct rebalancing

		while (true) {
			if (node == head) { // Reach head before rebalancing
				return;
			}

			AVLTreeValue::update_height(node);
			if (this->try_rebalance(node)) { // Rebalance, stop trying
				break;
			}
			node = node->parent;
		}

		while ((node = node->parent) != head) { // Update the remaining nodes height
			AVLTreeValue::update_height(node);
		}
	}

	NodePointer insert(const NodeLocation<NodePointer> location, const NodePointer newNode) noexcept {
		// Insert newNode at location
		++size;
		if (!location.parent) { // First node in tree
			newNode->parent = head;
			head->left = newNode;
			head->right = newNode;
			head->parent = newNode;
			return newNode;
		}

		newNode->parent = location.parent;
		if (location.child == NodeChild::LEFT) { // Insert as left child
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

		this->fixTree(location.parent, newNode);
		return newNode;
	}

	std::pair<NodePointer, NodePointer> extract(const AVLTreeConstIterator<AVLTreeValue> pos) noexcept {
		// Extract node pointed by pos
		--size;
		const NodePointer extracted = pos.getPointer(); // UB: pos == AVLTree::end()
		const NodePointer nextNode = std::next(pos, 1).getPointer();
		if (size == 0) { // Extract final node
			head->left = nullptr;
			head->right = nullptr;
		}
		else if (extracted == head->left) { // Extract leftmost node
			head->left = nextNode;
		}
		else if (extracted == head->right) { // Extract rightmost node
			head->right = std::prev(pos, 1).getPointer();
		}

		const NodePointer parent = extracted->parent;
		if (extracted->left && extracted->right) { // Node has both children
			const NodePointer successor = this->min(extracted->right);
			successor->parent->release_child(successor);
			extracted->parent->replace_child(extracted, successor);

			if (extracted->left) { // Adopt extracted's left child
				extracted->left->parent = successor;
				successor->left = std::exchange(extracted->left, nullptr);
			}

			if (extracted->right) { // Adopt extracted's right child
				extracted->right->parent = successor;
				successor->right = std::exchange(extracted->right, nullptr);
			}
			extracted->parent = successor; // Fix tree starting point
		}
		else if (!extracted->left && !extracted->right) { // Extract leaf node
			parent->release_child(extracted);
		}
		else { // Node has a single child
			const NodePointer childNode = std::exchange((extracted->left) ? extracted->left : extracted->right, nullptr);
			parent->replace_child(extracted, childNode);
		}

		this->fixTree(std::exchange(extracted->parent, nullptr), extracted);
		return std::make_pair(extracted, nextNode);
	}

	void clear(NodePointer node) noexcept {
		// Clear entire subtree at node recursively
		while (node) {
			this->clear(node->right);
			NodeType::free_node(std::exchange(node, node->left));
		}
	}

	void swap(AVLTreeValue& other) noexcept {
		using std::swap; // ADL
		swap(head, other.head);
		swap(size, other.size);
	}

	/*
		Node head serves as the end() node for tree traversal

		head->left:		points to the leftmost node (min node)
		head->right:	points to the rightmost node (max node)
		head->parent:	points to the actual root node
	*/
	NodePointer head;

	SizeType size;
};

template<class T, class Comp = std::less<>>
class AVLTree {
public:
	using value_type = T;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using pointer = T*;
	using const_pointer = const T*;
	using reference = T&;
	using const_reference = const T&;

private:
	using _NodeType = AVLTreeNode<value_type, uint8_t, int8_t>;
	using _NodePointer = typename _NodeType::NodePointer;

	using _AVLTreeValue = AVLTreeValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

public:
	using iterator = AVLTreeConstIterator<_AVLTreeValue>;
	using const_iterator = AVLTreeConstIterator<_AVLTreeValue>;

	AVLTree()
		: _data() {
		// Construct empty tree
		_data.head = _NodeType::construct_head();
	}

	AVLTree(const AVLTree& other)
		: _data() {
		// Construct tree by copying from other
		_data.head = _NodeType::construct_head();
		this->_copy(other);
	}

	AVLTree& operator=(const AVLTree& other) {
		if (this != std::addressof(other)) {
			this->clear();
			this->_copy(other);
		}
		return *this;
	}

	AVLTree(AVLTree&& other) noexcept
		: _data() {
		// Construct tree by moving from other
		_data.head = _NodeType::construct_head();
		_data.swap(other._data);
	}

	AVLTree& operator=(AVLTree&& other) noexcept {
		if (this != std::addressof(other)) {
			this->clear();
			_data.swap(other._data);
		}
		return *this;
	}

	~AVLTree() noexcept {
		_data.clear(_data.head->parent);
		_NodeType::free_empty_node(_data.head);
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

	[[nodiscard]] reference min() noexcept {
		return _data.head->left->value; // UB
	}

	[[nodiscard]] const_reference min() const noexcept {
		return _data.head->left->value; // UB
	}

	[[nodiscard]] reference max() noexcept {
		return _data.head->right->value; // UB
	}

	[[nodiscard]] const_reference max() const noexcept {
		return _data.head->right->value; // UB
	}

	[[nodiscard]] size_type size() const noexcept {
		return _data.size;
	}

	[[nodiscard]] size_type max_size() const noexcept {
		return static_cast<size_type>(-1) / sizeof(_NodeType);
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _data.size == 0;
	}

	template<class... Args>
	std::pair<iterator, bool> emplace(Args&&... args) {
		// Insert by constructing in place using args
		const auto result = this->_emplace(std::forward<Args>(args)...);
		return std::make_pair(iterator(result.first), result.second);
	}

	template<class... Args>
	iterator emplace_hint(const_iterator hint, Args&&... args) {
		// Insert with hint by constructing in place using args
		return iterator(this->_emplace_hint(hint.getPointer(), std::forward<Args>(args)...));
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
	void insert(It first, const It last) {
		// Insert range [first, last)
		for (; first != last; ++first) {
			this->_emplace_hint(_data.head, *first);
		}
	}

	void insert(std::initializer_list<value_type> initList) {
		// Insert initList
		this->insert(initList.begin(), initList.end());
	}

	iterator erase(const_iterator pos) noexcept {
		// Erase at pos
		return iterator(this->_erase(pos)); // UB
	}

	iterator erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		return iterator(this->_erase(first, last)); // UB
	}

	bool erase(const value_type& key) noexcept {
		// Erase key
		const auto result = this->_find_lower_bound(key);
		if (!this->_is_duplicate_key(result.bound, key)) { // Key does not exist
			return false;
		}
		this->_erase(const_iterator(result.bound));
		return true;
	}

	/*
		STL requires C++23 for heterogeneous erase
		Use with caution

		Intentional SFINAE with Comp
	*/
	template<class KeyT, class Comp2 = Comp>
		requires requires {
			typename Comp2::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	bool erase(const KeyT& key) noexcept {
		// Erase key
		const auto result = this->_find_lower_bound(key);
		if (!this->_is_duplicate_key(result.bound, key)) { // Key does not exist
			return false;
		}
		this->_erase(const_iterator(result.bound));
		return true;
	}

	void clear() noexcept {
		// Erase all
		_data.clear(std::exchange(_data.head->parent, nullptr));
		_data.head->left = _data.head;
		_data.head->right = _data.head;
		_data.size = 0;
	}

	void swap(AVLTree& other) noexcept {
		// Swap contents with other
		using std::swap;
		if (this != std::addressof(other)) {
			swap(_data.head, other._data.head); // ADL
			std::swap(_data.size, other._data.size);
		}
	}

	[[nodiscard]] iterator find(const value_type& key) {
		// Find key
		return iterator(this->_find(key));
	}

	[[nodiscard]] const_iterator find(const value_type& key) const {
		// Find key
		return const_iterator(this->_find(key));
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] iterator find(const KeyT& key) {
		// Find element equivalent to key
		return iterator(this->_find(key));
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] const_iterator find(const KeyT& key) const {
		// Find element equivalent to key
		return const_iterator(this->_find(key));
	}

	[[nodiscard]] bool contains(const value_type& key) const {
		// Check if tree contains key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] bool contains(const KeyT& key) const {
		// Check if tree contains element equivalent to key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	[[nodiscard]] size_type count(const value_type& key) const {
		// Count occurrences of key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] size_type count(const KeyT& key) const {
		// Count occurrences of value equivalent to key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	[[nodiscard]] iterator lower_bound(const value_type& key) {
		// Find the first element not less than key
		return iterator(this->_find_lower_bound(key).bound);
	}

	[[nodiscard]] const_iterator lower_bound(const value_type& key) const {
		// Find the first element not less than key
		return const_iterator(this->_find_lower_bound(key).bound);
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] iterator lower_bound(const KeyT& key) {
		// Find the first equivalent element not less than key
		return iterator(this->_find_lower_bound(key).bound);
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] const_iterator lower_bound(const KeyT& key) const {
		// Find the first equivalent element not less than key
		return const_iterator(this->_find_lower_bound(key).bound);
	}

	[[nodiscard]] iterator upper_bound(const value_type& key) {
		// Find the first element greater than key
		return iterator(this->_find_upper_bound(key).bound);
	}

	[[nodiscard]] const_iterator upper_bound(const value_type& key) const {
		// Find the first element greater than key
		return const_iterator(this->_find_upper_bound(key).bound);
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] iterator upper_bound(const KeyT& key) {
		// Find the first equivalent element greater than key
		return iterator(this->_find_upper_bound(key).bound);
	}

	template<class KeyT, class Comp2 = Comp>
		requires requires {
		typename Comp2::is_transparent;
		requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
		requires !concepts::implicitly_convertible_to<KeyT, iterator>;
	}
	[[nodiscard]] const_iterator upper_bound(const KeyT& key) const {
		// Find the first equivalent element greater than key
		return const_iterator(this->_find_upper_bound(key).bound);
	}

	template<class, class>
	friend class AVLTree;

	template<class Comp2>
	void merge(AVLTree<T, Comp2>& other) {
		// Merge other into *this, leaving other empty
		if constexpr (std::is_same_v<AVLTree, AVLTree<T, Comp2>>) {
			if (this == std::addressof(other)) {
				return;
			}
		}

		for (auto iter = other.begin(); iter != other.end();) {
			const auto currNode = iter.ptr;
			++iter; // Important: increment iterator before extraction

			NodeFindResult<_NodePointer> result = this->_find_lower_bound(currNode->value);
			if (this->_is_duplicate_key(result.bound, currNode->value)) {
				continue;
			}

			this->_check_max_size();
			// Extract from other and reset links
			const auto extracted = other._data.extract(const_iterator(currNode)).first;
			extracted->left = nullptr;
			extracted->right = nullptr;
			// Insert back into *this
			_data.insert(result.location, extracted); // Handle extracted->parent and extracted->height
		}
	}

	template<class Comp2>
	void merge(AVLTree<T, Comp2>&& other) {
		// Merge other into *this, leaving other empty
		this->merge(other);
	}

#if _MSVC_LANG >= 201703L
	using NodeHandle = NodeHandle<_NodeType, NodeHandleSetBase, value_type>;

	NodeHandle extract(const const_iterator pos) {
		// Extract node at pos, return its NodeHandle
		const auto result = _data.extract(pos);
		return NodeHandle::make(result.first);
	}

	NodeHandle extract(const value_type& key) {
		// Extract node with key, return its NodeHandle
		const const_iterator pos = this->find(key);
		if (pos == end()) {
			return NodeHandle{};
		}
		return this->extract(pos);
	}

	auto insert(NodeHandle&& handle) {
		// Insert node from handle
		if (handle.isEmpty()) {
			return InsertReturnType<iterator, NodeHandle>{end(), false, {}};
		}

		const auto node = handle.getPointer();
		NodeFindResult<_NodePointer> result = this->_find_lower_bound(node->value);
		if (this->_is_duplicate_key(result.bound, node->value)) {
			return InsertReturnType<iterator, NodeHandle>{iterator(result.bound), false, std::move(handle)};
		}

		this->_check_max_size();

		node->left = nullptr;
		node->right = nullptr;
		const auto inserted = _data.insert(result.location, handle._release());
		return InsertReturnType<iterator, NodeHandle>{iterator(inserted), true, std::move(handle)};
	}

	iterator insert(const const_iterator hint, NodeHandle&& handle) {
		// Insert node from handle with hint
		if (handle.isEmpty()) {
			return end();
		}
		const auto node = handle.getPointer();
		NodeFindHintResult<_NodePointer> result = this->_find_hint(hint.getPointer(), node->value);
		if (result.isDuplicate) {
			return iterator(result.location.parent);
		}

		this->_check_max_size();

		node->left = nullptr;
		node->right = nullptr;
		const auto inserted = _data.insert(result.location, handle._release());
		return iterator(inserted);
	}
#endif // Has C++17

	//struct DefaultPrint {
	//	// Default print functor
	//	template<class NodePointer>
	//	void operator()(NodePointer node) const noexcept {
	//		std::cout << node->value << " ";
	//	}
	//};

	//template<class PrintFnc = DefaultPrint, Sep>
	//void print(const TreeOrder order, PrintFnc print = PrintFnc{}) {
	//	// Print tree in specified order using print function
	//	const _NodePointer root = _data.head->parent;
	//	if (!root) {
	//		return;
	//	}

	//	switch (order) {
	//		case PRE_ORDER: {
	//			this->_pre_order(root, print);
	//			break;
	//		}
	//		case IN_ORDER: {
	//			this->_in_order(root, print);
	//			break;
	//		}
	//		case POST_ORDER: {
	//			this->_post_order(root, print);
	//			break;
	//		}
	//		case LEVEL_ORDER: {
	//			this->_level_order(root, print);
	//			break;
	//		}
	//	}
	//	std::cout << "\n";
	//}

private:
	_NodePointer _copy_node(value_type& val) {
		// Construct node by copying val
		return _NodeType::construct_node(val);
	}

	_NodePointer _copy_subtree(_NodePointer oldRoot, _NodePointer newHead) {
		// Copy subtree at oldRoot into newHead recursively
		if (oldRoot == nullptr) {
			return nullptr;  // Return nullptr for empty subtree
		}

		_NodePointer newRoot = this->_copy_node(oldRoot->value);

		newRoot->parent = newHead;
		newRoot->height = oldRoot->height;
		newRoot->left = this->_copy_subtree(oldRoot->left, newRoot);
		newRoot->right = this->_copy_subtree(oldRoot->right, newRoot);
		return newRoot;
	}

	void _copy(const AVLTree& other) {
		// Copy entire tree from other
		_data.head->parent = this->_copy_subtree(other._data.head->parent, _data.head);
		_data.size = other._data.size;
		// Update leftmost and rightmost nodes
		if (_data.head->parent == nullptr) { // Empty tree
			_data.head->left = _data.head;
			_data.head->right = _data.head;
		}
		else { // Non-empty tree, find min and max
			_data.head->left = _AVLTreeValue::min(_data.head->parent);
			_data.head->right = _AVLTreeValue::max(_data.head->parent);
		}
	}

	template<class KeyT>
	[[nodiscard]] bool _is_duplicate_key(const _NodePointer bound, const KeyT& key) const {
		// Check if key is duplicate by comparing with bound
		return !bound->isHead && !(_comp(key, bound->value));
	}

	template<class KeyT>
	[[nodiscard]] NodeFindResult<_NodePointer> _find_lower_bound(const KeyT& key) const {
		/*
			Find the smallest (or leftmost in-order) node that is not less than key (or does not satisfy _comp(node value, key))

			Traverse the whole path downwards from root until nullptr is reached.
			At each node, perform exactly 01 comparison using _comp::operator() on key and node value.

			Let N be the total number of nodes:
			Best case:		O(1),		using 01 comparison (root case)
			Worst case:		O(log2(N)), using log2(N) comparisons
			Average case:	O(log2(N))
		*/
		NodeFindResult<_NodePointer> result{ _data.head, { _data.head->parent, NodeChild::RIGHT } };
		for (_NodePointer currNode = result.location.parent; currNode;) {
			result.location.parent = currNode;
			if (_comp(currNode->value, key)) {
				result.location.child = NodeChild::RIGHT;
				currNode = currNode->right;
			}
			else {
				result.location.child = NodeChild::LEFT;
				result.bound = currNode;
				currNode = currNode->left;
			}
		}
		return result;
	}

	template<class KeyT>
	[[nodiscard]] NodeFindResult<_NodePointer> _find_upper_bound(const KeyT& key) const {
		// Find the smallest (or leftmost in-order) node that is strictly greater than key (or satisfies _comp(key, node value))
		NodeFindResult<_NodePointer> result{ _data.head, { _data.head->parent, NodeChild::RIGHT } };
		for (_NodePointer currNode = result.location.parent; currNode;) {
			result.location.parent = currNode;
			if (_comp(key, currNode->value)) {
				result.location.child = NodeChild::LEFT;
				result.bound = currNode;
				currNode = currNode->left;
			}
			else {
				result.location.child = NodeChild::RIGHT;
				currNode = currNode->right;
			}
		}
		return result;
	}

	template<class KeyT>
	[[nodiscard]] NodeFindHintResult<_NodePointer> _find_hint(const _NodePointer hintNode, const KeyT& key) const {
		// Find node insert location using hintNode
		const _NodePointer head = _data.head;
		if (hintNode == head->left) { // Insert at begin as leftmost node
			if (_comp(key, hintNode->value)) {
				return { { hintNode, NodeChild::LEFT }, false };
			}
		}
		else if (hintNode->isHead) { // Insert at end as rightmost node
			if (!head->parent || _comp(head->right->value, key)) {
				return { { head->right, NodeChild::RIGHT }, false };
			}
		}
		else if (_comp(key, hintNode->value)) { // key < *hintNode
			const _NodePointer prevNode = std::prev(const_iterator(hintNode), 1).getPointer();
			if (_comp(prevNode->value, key)) { // *(--hintNode) < key < *hintNode, insert here
				if (!prevNode->right) {
					return { { prevNode, NodeChild::RIGHT }, false };
				}
				return { { hintNode, NodeChild::LEFT }, false };
			}
		}
		else if (_comp(hintNode->value, key)) { // key > *hintNode
			const _NodePointer nextNode = std::next(const_iterator(hintNode), 1).getPointer();
			if (nextNode->isHead || _comp(key, nextNode->value)) { // *hintNode < key < *(++hintNode), insert here
				if (!hintNode->right) {
					return { { hintNode, NodeChild::RIGHT }, false };
				}
				return { { nextNode, NodeChild::LEFT }, false };
			}
		}
		else { // Duplicate value, don't insert
			return { { hintNode, NodeChild::LEFT, }, true };
		}
		// Incorrect hint, key is not in the proximity of *hintNode. Resort to the usual find method
		const auto result = this->_find_lower_bound(key);
		if (this->_is_duplicate_key(result.bound, key)) {
			return { { result.bound, NodeChild::LEFT }, true };
		}
		return { result.location, false };
	}

	template<class... Args>
	std::pair<_NodePointer, bool> _emplace(Args&&... args) {
		// Insert by constructing node inplace using args
		AVLTreeTempNode<_NodeType> tempNode(std::forward<Args>(args)...); // Create temporary node for initial node search
		const auto& key = tempNode.get_value();

		const auto result = this->_find_lower_bound(key); // Find insert location
		if (this->_is_duplicate_key(result.bound, key)) { // Duplicate check
			return std::make_pair(result.bound, false);
		}

		this->_check_max_size();

		const _NodePointer newNode = tempNode.release(); // Safe to insert, release temp node, transfer ownership to *this
		return std::make_pair(_data.insert(result.location, newNode), true);
	}

	template<class... Args>
	_NodePointer _emplace_hint(const _NodePointer hintNode, Args&&... args) {
		// Insert by constructing node inplace using args with given hint
		AVLTreeTempNode<_NodeType> tempNode(std::forward<Args>(args)...);
		const auto& key = tempNode.get_value();

		const auto result = this->_find_hint(hintNode, key);
		if (result.isDuplicate) {
			return result.location.parent;
		}

		this->_check_max_size();

		const _NodePointer newNode = tempNode.release();
		return _data.insert(result.location, newNode);
	}

	_NodePointer _erase(const_iterator pos) noexcept {
		// Erase node at pos, return the next in-order node
		const auto result = _data.extract(pos); // UB
		_NodeType::free_node(result.first);
		return result.second;
	}

	_NodePointer _erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		const auto begin = this->begin();
		if (first == this->begin() && last == this->end()) { // Erase all elements
			this->clear();
			return last.getPointer();
		}
		// Erase nodes one at a time
		while (first != last) {
			this->_erase(first++); // UB
		}
		return last.getPointer();
	}

	template<class KeyT>
	[[nodiscard]] _NodePointer _find(const KeyT& key) const {
		// Find element equivalent to key
		const auto result = this->_find_lower_bound(key);
		if (this->_is_duplicate_key(result.bound, key)) {
			return result.bound;
		}
		return _data.head;
	}

	void _check_max_size() {
		// Check if tree has reached max size
		if (this->max_size() == _data.size) {
			throw std::length_error("container reached max size");
		}
	}

	//template<class PrintFnc>
	//void _pre_order(_NodePointer node, PrintFnc print) {
	//	// Print subtree at node in pre-order
	//	if (!node) {
	//		return;
	//	}

	//	print(node);
	//	this->_pre_order(node->left, print);
	//	this->_pre_order(node->right, print);
	//}

	//template<class PrintFnc>
	//void _in_order(_NodePointer node, PrintFnc print) {
	//	// Print subtree at node in in-order
	//	if (!node) {
	//		return;
	//	}

	//	this->_in_order(node->left, print);
	//	print(node);
	//	this->_in_order(node->right, print);
	//}

	//template<class PrintFnc>
	//void _post_order(_NodePointer node, PrintFnc print) {
	//	// Print subtree at node in post-order
	//	if (!node) {
	//		return;
	//	}

	//	this->_post_order(node->left, print);
	//	this->_post_order(node->right, print);
	//	print(node);
	//}

	//template<class PrintFnc>
	//void _level_order(_NodePointer root, PrintFnc print) {
	//	// Print subtree at node in level-order
	//	if (!root) {
	//		return;
	//	}

	//	std::queue<_NodePointer> nodesQueue;
	//	nodesQueue.push(root);
	//	while (!nodesQueue.is_empty()) {
	//		const _NodePointer node = nodesQueue.front();
	//		print(node);

	//		nodesQueue.pop();
	//		if (node->left) {
	//			nodesQueue.push(node->left);
	//		}
	//		if (node->right) {
	//			nodesQueue.push(node->right);
	//		}
	//	}
	//}

private:
	_AVLTreeValue	_data;
	Comp			_comp;
};
#endif // ALV_TREE_H