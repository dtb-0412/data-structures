#pragma once
#ifndef ALV_TREE_H
#define ALV_TREE_H

#include<iostream>
#include<queue>

#include"compare.hpp"
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
		return ptr->value;
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
	}

	_AVLTreeConstIterator& operator++() noexcept {
		if (ptr->right->isNil) { // Climb upwards, look for right subtree
			for (_NodePointer currNode = ptr->parent;;) {
				// Stop when reaching head or going upwards from a left subtree for the first time
				if (currNode->isNil || ptr != currNode->right) {
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
		if (ptr->isNil) { // Goes back from end() to the rightmost node
			ptr = ptr->right;
		}
		else if (ptr->left->isNil) {
			for (_NodePointer currNode = ptr->parent;;) {
				// Stop when reaching head or going upwards from a right subtree for the first time
				if (currNode->isNil || ptr != currNode->left) {
					if (!ptr->isNil) {
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
		return const_cast<reference>(_BaseIter::operator*());
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this));
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
		newHead->isNil = true;
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
		guard.node->isNil = false;
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
		if (isNil) {
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

	node_pointer left;		// 8 bytes pointer
	node_pointer right;		// 8 bytes pointer
	node_pointer parent;	// 8 bytes pointer

	value_type	value;		// sizeof(value_type)
	height_type	height;		// 1 byte, assuming AVL tree height <= 255

	bool isNil; // 1 byte boolean, whether node is head sentinel or child of leaf nodes
};

enum _NodeChild : bool {
	LEFT, RIGHT, UNUSED
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
		while (!node->left->isNil) {
			node = node->left;
		}
		return node;
	}

	[[nodiscard]] static node_pointer max(node_pointer node)  noexcept {
		// Get the rightmost node in subtree at node
		while (!node->right->isNil) {
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
		if (!node->isNil) {
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

		if (!child->isNil) { // Reattach newRoot's left child to oldRoot
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

		if (!child->isNil) { // Reattach newRoot's right child to oldRoot
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
			if (node->isNil) { // Reach head before rebalancing
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
			if (node->isNil) {
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

	node_pointer extract(_AVLTreeConstIterator<_AVLTreeValue> where) noexcept {
		// Extract node pointed by where
		--size;
		const node_pointer extracted = where.ptr; // UB: where == _AVLTree::end()
		if (size == 0) { // Extract final node
			head->left	= head;
			head->right = head;
		}
		else if (extracted == head->left) { // Extract leftmost node
			head->left = (++_AVLTreeConstIterator(where)).ptr;
		}
		else if (extracted == head->right) { // Extract rightmost node
			head->right = (--_AVLTreeConstIterator(where)).ptr;
		}

		node_pointer parent = extracted->parent;
		if (!(extracted->left->isNil || extracted->right->isNil)) { // Node has both children
			const node_pointer successor = this->min(extracted->right);
			successor->parent->replace_child(successor, head);
			extracted->parent->replace_child(extracted, successor);

			if (!extracted->left->isNil) { // Adopt extracted's left child
				extracted->left->parent = successor;
				successor->left = std::exchange(extracted->left, head);
			}

			if (!extracted->right->isNil) { // Adopt extracted's right child
				extracted->right->parent = successor;
				successor->right = std::exchange(extracted->right, head);
			}
			extracted->parent = successor; // Fix tree starting point
		}
		else if (extracted->left->isNil && extracted->right->isNil) { // Extract leaf node
			parent->replace_child(extracted, head);
		}
		else { // Node has a single child
			const node_pointer childNode = std::exchange(
				(extracted->left->isNil) ? extracted->right : extracted->left, head
			);
			parent->replace_child(extracted, childNode);
		}

		this->fix_tree(std::exchange(extracted->parent, head), extracted);
		return extracted;
	}

	void clear_subtree(node_pointer node) noexcept {
		// Clear entire subtree at node recursively
		while (!node->isNil) {
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
		base.node->isNil = false;
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
	using iterator			= std::conditional_t<_isMap, _AVLTreeIterator<_MyVal>, _AVLTreeConstIterator<_MyVal>>;
	using const_iterator	= _AVLTreeConstIterator<_MyVal>;

	using reverse_iterator			= std::reverse_iterator<iterator>;
	using const_reverse_iterator	= std::reverse_iterator<const_iterator>;

	using node_handle			= typename Traits::node_handle;
	using insert_return_type	= _InsertReturnType<iterator, node_handle>;
	
public:
	_AVLTree()
		: _data(), _comp() {
		_data.head = _NodeType::construct_head();
	}
	
	template<std::input_iterator It>
		requires std::sentinel_for<It, It>
	_AVLTree(It first, It last)
		: _data(), _comp() {
		_TreeConstructGuard<_MyVal> guard(_data);
		this->insert(first, last);
		guard.release();
	}

	_AVLTree(std::initializer_list<value_type> initList)
		: _data(), _comp() {
		_TreeConstructGuard<_MyVal> guard(_data);
		this->insert(initList);
		guard.release();
	}

	_AVLTree(const _AVLTree& other)
		: _data(), _comp(other._comp) {
		_TreeConstructGuard<_MyVal> guard(_data);
		this->_copy<_CopyStrategy::Copy>(other);
		guard.release();
	}

	_AVLTree(_AVLTree&& other) noexcept
		: _data(), _comp(other._comp) { // Intentionally copy _comp
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
			_comp = other._comp;
		}
		return *this;
	}

	_AVLTree& operator=(_AVLTree&& other) noexcept {
		if (this != std::addressof(other)) {
			this->clear();
			_data.swap(other._data);
			_comp = other._comp;
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

	[[nodiscard]] key_compare key_comp() const {
		return _comp;
	}

	[[nodiscard]] value_compare value_comp() const {
		return value_compare(this->key_comp());
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

	std::pair<iterator, bool> insert(const value_type& val)
		requires (!_isMulti)
	{
		// Insert by copying val
		const auto result = this->_emplace(val);
		return { iterator(result.first), result.second };
	}

	iterator insert(const value_type& val)
		requires (_isMulti)
	{
		// Insert by copying val
		return iterator(this->emplace(val).first);
	}

	std::pair<iterator, bool> insert(value_type&& val)
		requires (!_isMulti)
	{
		// Insert by moving val
		const auto result = this->_emplace(std::move(val));
		return { iterator(result.first), result.second };
	}

	iterator insert(value_type&& val)
		requires (_isMulti)
	{
		// Insert by moving val
		return iterator(this->emplace(std::move(val)).first);
	}

	iterator insert(const_iterator hint, const value_type& val) {
		// Insert with hint by copying val
		return iterator(this->_emplace_hint(hint.ptr, val));
	}

	iterator insert(const_iterator hint, value_type&& val) {
		// Insert with hint by moving val
		return iterator(this->_emplace_hint(hint.ptr, std::move(val)));
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

	iterator erase(iterator where) noexcept
		requires (_isMap)
	{
		// Erase at where
		return iterator(this->_erase(where));
	}

	iterator erase(const_iterator where) noexcept {
		// Erase at where
		return iterator(this->_erase(where));
	}

	iterator erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		return iterator(this->_erase(first, last));
	}

	size_type erase(const key_type& key)
		noexcept(noexcept(_equal_range(key)))
	{
		// Erase all occurences of key
		return this->_erase(this->_equal_range(key));
	}

	/*
		STL requires C++23 for heterogeneous look up.
		Use with caution.
	*/
	template<class KeyT, class Comp = key_compare>
		requires requires {
			typename Comp::is_transparent;
			requires !concepts::implicitly_convertible_to<KeyT, const_iterator>;
			requires !concepts::implicitly_convertible_to<KeyT, iterator>;
		}
	size_type erase(KeyT&& key)
		noexcept(noexcept(_equal_range(key)))
	{
		// Erase all elements equivalent to key
		return this->_erase(this->_equal_range(key));
	}

	void clear() noexcept {
		// Erase all elements
		_data.clear_subtree(_data.head->parent);
		_data.head->left	= _data.head;
		_data.head->right	= _data.head;
		_data.head->parent	= _data.head;
		_data.size			= 0;
	}

	void swap(_AVLTree& other) noexcept {
		// Swap with other
		using std::swap;
		if (this != std::addressof(other)) {
			_data.swap(other._data);
			swap(_comp, other._comp);
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

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] iterator find(const KeyT& key) {
		// Find the first element equivalent to key
		return iterator(this->_find(key));
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] const_iterator find(const KeyT& key) const {
		// Find the first element equivalent to key
		return const_iterator(this->_find(key));
	}

	[[nodiscard]] bool contains(const key_type& key) const {
		// Check if tree contains key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] bool contains(const KeyT& key) const {
		// Check if tree contains element equivalent to key
		return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
	}

	[[nodiscard]] size_type count(const key_type& key) const {
		// Count occurrences of key
		if constexpr (_isMulti) {
			const auto result = this->_equal_range(key);
			return static_cast<size_type>(std::distance(
				const_iterator(result.first), const_iterator(result.second)
			));
		}
		else {
			return this->_is_duplicate_key(this->_find_lower_bound(key).bound, key);
		}
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] size_type count(const KeyT& key) const {
		// Count occurrences of elements equivalent to key
		const auto result = this->_equal_range(key);
		return static_cast<size_type>(std::distance(
			const_iterator(result.first), const_iterator(result.second)
		));
	}

	[[nodiscard]] iterator lower_bound(const key_type& key) {
		// Find the first element not less than key
		return iterator(this->_find_lower_bound(key).bound);
	}

	[[nodiscard]] const_iterator lower_bound(const key_type& key) const {
		// Find the first element not less than key
		return const_iterator(this->_find_lower_bound(key).bound);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] iterator lower_bound(const KeyT& key) {
		// Find the first equivalent element not less than key
		return iterator(this->_find_lower_bound(key).bound);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
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

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] iterator upper_bound(const KeyT& key) {
		// Find the first equivalent element greater than key
		return iterator(this->_find_upper_bound(key).bound);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] const_iterator upper_bound(const KeyT& key) const {
		// Find the first equivalent element greater than key
		return const_iterator(this->_find_upper_bound(key).bound);
	}

	[[nodiscard]] std::pair<iterator, iterator> equal_range(const key_type& key) {
		// Find the range of elements equivalent to key
		const auto result = this->_equal_range(key);
		return { iterator(result.first), iterator(result.second) };
	}

	[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const {
		// Find the range of elements equivalent to key
		const auto result = this->_equal_range(key);
		return { const_iterator(result.first), const_iterator(result.second) };
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] std::pair<iterator, iterator> equal_range(const KeyT& key) {
		// Find the range of elements equivalent to key
		const auto result = this->_equal_range(key);
		return { iterator(result.first), iterator(result.second) };
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(const KeyT& key) const {
		// Find the range of elements equivalent to key
		const auto result = this->_equal_range(key);
		return { const_iterator(result.first), const_iterator(result.second) };
	}

	template<class>
	friend class _AVLTree;

	template<class OtherTraits>
	void merge(_AVLTree<OtherTraits>& other) {
		// Merge other into *this, leaving other empty
		if constexpr (std::is_same_v<_AVLTree, _AVLTree<OtherTraits>>) {
			if (this == std::addressof(other)) {
				return;
			}
		}

		for (auto iter = other.begin(); iter != other.end();) {
			const _NodePointer currNode = iter.ptr;
			++iter; // Important: increment iterator before extraction

			const auto& key = Traits::key_from_node(currNode->value);

			_NodeFindResult<_NodePointer> result;
			if constexpr (_isMulti) {
				result = this->_find_upper_bound(key);
			}
			else {
				result = this->_find_lower_bound(key);
				if (this->_is_duplicate_key(result.bound, key)) {
					continue;
				}
			}

			if (this->max_size() == _data.size) {
				this->_length_error();
			}
			// Extract from other and reset links
			const auto extracted	= other._data.extract(const_iterator(currNode));
			extracted->left			= _data.head;
			extracted->right		= _data.head;
			// Insert back into *this
			_data.insert(result.location, extracted); // Handle extracted->parent and extracted->height
		}
	}

	template<class OtherTraits>
	void merge(_AVLTree<OtherTraits>&& other) {
		// Merge other into *this, leaving other empty
		this->merge(other);
	}

	node_handle extract(const_iterator where) {
		// Extract node at where, return its node_handle
		return node_handle::make(_data.extract(where));
	}

	node_handle extract(const key_type& key) {
		// Extract node with key, return its node_handle
		const auto where = this->find(key);
		if (where == this->end()) {
			return node_handle{};
		}
		return this->extract(where);
	}

	auto insert(node_handle&& handle) {
		// Insert node from handle
		if (handle.is_empty()) {
			if constexpr (_isMulti) {
				return this->end();
			}
			else {
				return insert_return_type{ this->end(), false, node_handle{} };
			}
		}

		const auto node = handle.get_pointer();
		const auto& key = Traits::key_from_node(node->value);

		_NodeFindResult<_NodePointer> result;
		if constexpr (_isMulti) {
			result = this->_find_upper_bound(key);
		}
		else {
			result = this->_find_lower_bound(key);
			if (this->_is_duplicate_key(result.bound, node->value)) {
				return insert_return_type{ iterator(result.bound), false, std::move(handle) };
			}
		}

		if (this->max_size() == _data.size) {
			this->_length_error();
		}

		node->left	= _data.head;
		node->right = _data.head;
		
		const auto inserted = _data.insert(result.location, handle._release());
		if constexpr (_isMulti) {
			return iterator(inserted);
		}
		else {
			return insert_return_type{ iterator(inserted), true, std::move(handle) };
		}
	}

	iterator insert(const_iterator hint, node_handle&& handle) {
		// Insert node from handle with hint
		if (handle.is_empty()) {
			return this->end();
		}

		const auto node		= handle.get_pointer();
		const auto& key		= Traits::key_from_node(node->value);
		const auto result	= this->_find_hint(hint.ptr, node->value);
		if (result.isDuplicate) {
			return iterator(result.location.parent);
		}

		if (this->max_size() == _data.size) {
			this->_length_error();
		}

		node->left	= _data.head;
		node->right = _data.head;
		return iterator(_data.insert(result.location, handle._release()));
	}

	void level_order() {
		// Print subtree at node in level-order
		_NodePointer root = _data.head->parent;
		if (root->isNil) {
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
			if (!node->left->isNil) {
				nodesQueue.push(node->left);
				if (isBound) {
					bound = node->left;
				}
			}
			if (!node->right->isNil) {
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
			if constexpr (_isMap) {
				return _NodeType::construct_node(const_cast<key_type&>(val.first), std::move(val.second));
			}
			else {
				return _NodeType::construct_node(_data.head, std::move(val));
			}
		}
	}

	template<_CopyStrategy _strat>
	_NodePointer _copy_subtree(_NodePointer oldRoot, _NodePointer where) {
		// Copy subtree at oldRoot into where recursively
		_NodePointer newRoot = _data.head;
		if (!oldRoot->isNil) {
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
		if (_data.head->parent->isNil) { // Empty tree
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
		return !bound->isNil && !(_comp(key, Traits::key_from_node(bound->value)));
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
		
		_NodePointer currNode = result.location.parent;
		while(!currNode->isNil) {
			result.location.parent = currNode;
			if (_comp(Traits::key_from_node(currNode->value), key)) {
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
		
		_NodePointer currNode = result.location.parent;
		while(!currNode->isNil) {
			result.location.parent = currNode;
			if (_comp(key, Traits::key_from_node(currNode->value))) {
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
		if constexpr (_isMulti) {
			if (hintNode->isNil) {
				// Insert at end if >= last element
				if (head->parent->isNil || _comp(key, Traits::key_from_node(head->right->value))) {
					return { { head->right, _NodeChild::RIGHT }, false };
				}
				// hintNode is this->end(), it must be closer to the end of equivalent nodes
				return { this->_find_upper_bound(key).location, false };
			}

			if (hintNode == head->left) {
				// Insert at begin if <= first element
				if (!_comp(Traits::key_from_node(hintNode->value), key)) {
					return { { hintNode, _NodeChild::LEFT }, false };
				}
				// hintNode is this->begin(), it must be closer to the beginning of equivalent nodes
				return { this->_find_lower_bound(key).location, false };
			}

			if (!_comp(Traits::key_from_node(hintNode->value), key)) {
				// key <= *hintNode
				const _NodePointer prevNode = (--const_iterator(hintNode)).ptr;
				if (_comp(key, Traits::key_from_node(prevNode->value))) {
					// key <= *hintNode and key >= *prevNode, insert here
					if (prevNode->right->isNil) {
						return { { prevNode, _NodeChild::RIGHT } , false };
					}
					else {
						return { { hintNode, _NodeChild::LEFT }, false };
					}
				}
				// key goes before *hintNode, hintNode must be closer to the end of equivalent nodes
				return { this->_find_upper_bound(key).location, false };
			}
			// key goes after *hintNode, hintNode must be closer to the beginning of equivalent nodes
			return { this->_find_lower_bound(key).location, false };
		}
		else {
			if (hintNode->isNil) { // Insert at end if > last element
				if (head->parent->isNil || _comp(Traits::key_from_node(head->right->value), key)) {
					return { { head->right, _NodeChild::RIGHT }, false };
				}
			}
			else if (hintNode == head->left) { // Insert at begin if < first element
				if (_comp(key, Traits::key_from_node(hintNode->value))) {
					return { { hintNode, _NodeChild::LEFT }, false };
				}
			}
			else if (_comp(key, Traits::key_from_node(hintNode->value))) {
				// key < *hintNode
				const _NodePointer prevNode = (--const_iterator(hintNode)).ptr;
				if (_comp(Traits::key_from_node(prevNode->value), key)) {
					// key < *hintNode and key > *prevNode, insert here
					if (prevNode->right->isNil) {
						return { { prevNode, _NodeChild::RIGHT }, false };
					}
					else {
						return { { hintNode, _NodeChild::LEFT }, false };
					}
				}
			}
			else if (_comp(Traits::key_from_node(hintNode->value), key)) {
				// key > *hintNode
				const _NodePointer nextNode = (++const_iterator(hintNode)).ptr;
				if (nextNode->isNil || _comp(key, Traits::key_from_node(nextNode->value))) {
					// key > *hintNode and key < *nextNode, insert here
					if (hintNode->right->isNil) {
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
			const auto result = this->_find_lower_bound(key);
			if (this->_is_duplicate_key(result.bound, key)) {
				return { { result.bound, _NodeChild::UNUSED }, true };
			}
			return { result.location, false };
		}
	}

	template<class KeyT>
	std::pair<_NodePointer, _NodePointer> _equal_range(const KeyT& key) const
		noexcept(
			compare::is_nothrow_compare<key_compare, key_type, KeyT> &&
			compare::is_nothrow_compare<key_compare, KeyT, key_type>
		)
	{
		// Find the range of nodes equivalent to key
		_NodePointer currNode	= _data.head->parent;
		_NodePointer lowNode	= _data.head; // end() if search fails
		_NodePointer highNode	= _data.head; // end() if search fails

		while (!currNode->isNil) {
			if (_comp(Traits::key_from_node(currNode->value), key)) {
				currNode = currNode->right; // Descend right subtree
			}
			else { // currNode is not less than key, remember it
				if (highNode->isNil && _comp(key, Traits::key_from_node(currNode->value))) {
					highNode = currNode; // currNode is greater than key, remember it
				}

				lowNode		= currNode;
				currNode	= currNode->left; // Descend left subtree
			}
		}

		currNode = highNode->isNil ? _data.head->parent : highNode->left; // Continue searching for upper bound
		while (!currNode->isNil) {
			if (_comp(key, Traits::key_from_node(currNode->value))) { // currNode is greater than key, remember it
				highNode = currNode;
				currNode = currNode->left; // Descend left subtree
			}
			else {
				currNode = currNode->right; // Descend right subtree
			}
		}
		return { lowNode, highNode };
	}

	template<class... Args>
	std::pair<_NodePointer, bool> _emplace(Args&&... args) {
		// Insert by constructing node inplace using args
		using key_extractor = typename Traits::template in_place_key_extractor<Args...>;

		_NodePointer newNode;
		_NodeFindResult<_NodePointer> result;
		if constexpr (!_isMulti && key_extractor::isExtractable) {
			// Extract key from args and use it for searching without constructing a potentially unused node
			const auto& key = key_extractor::extract(args...);

			result = this->_find_lower_bound(key);
			if (this->_is_duplicate_key(result.bound, key)) { // Constructing a temporary node would be wasted here
				return { result.bound, false };
			}

			if (this->max_size() == _data.size) {
				this->_length_error();
			}
			newNode = _AVLTempNodeGuard<_NodeType>(_data.head, std::forward<Args>(args)...).release();
		}
		else {
			_AVLTempNodeGuard<_NodeType> guard(_data.head, std::forward<Args>(args)...); // Create temporary node for initial search
			
			const auto& key = Traits::key_from_node(guard.get_value());
			if constexpr (_isMulti) {
				result = this->_find_upper_bound(key);
			}
			else {
				result = this->_find_lower_bound(key); // Find insert location
				if (this->_is_duplicate_key(result.bound, key)) { // Duplicate check
					return { result.bound, false };
				}
			}

			if (this->max_size() == _data.size) {
				this->_length_error();
			}
			newNode = guard.release(); // Safe to insert, release temp node, transfer ownership to *this

		}
		return { _data.insert(result.location, newNode), true };
	}

	template<class... Args>
	_NodePointer _emplace_hint(_NodePointer hintNode, Args&&... args) {
		// Insert by constructing node inplace using args with given hint
		using key_extractor = typename Traits::template in_place_key_extractor<Args...>;

		_NodePointer newNode;
		_NodeFindHintResult<_NodePointer> result;
		if constexpr (!_isMulti && key_extractor::isExtractable) {
			result = this->_find_hint(hintNode, key_extractor::extract(args...));
			if (result.isDuplicate) {
				return result.location.parent;
			}

			if (this->max_size() == _data.size) {
				this->_length_error();
			}
			newNode = _AVLTempNodeGuard<_NodeType>(_data.head, std::forward<Args>(args)...).release();
		}
		else {
			_AVLTempNodeGuard<_NodeType> guard(_data.head, std::forward<Args>(args)...); // Create temporary node for initial search
			
			result = this->_find_hint(hintNode, Traits::key_from_node(guard.get_value()));
			if constexpr (!_isMulti) {
				if (result.isDuplicate) {
					return result.location.parent;
				}
			}

			if (this->max_size() == _data.size) {
				this->_length_error();
			}
			newNode = guard.release(); // Safe to insert, release temp node, transfer ownership to *this

		}
		return _data.insert(result.location, newNode);
	}

	_NodePointer _erase(const_iterator where) noexcept {
		// Erase node at where, return the next in-order node
		const auto next = (++const_iterator(where));
		_NodeType::free_node(_data.extract(where));
		return next.ptr;
	}

	_NodePointer _erase(const_iterator first, const_iterator last) noexcept {
		// Erase range [first, last)
		if (first == this->begin() && last == this->end()) { // Erase all elements
			this->clear();
			return last.ptr;
		}
		// Erase nodes one at a time
		while (first != last) {
			this->_erase(first++);
		}
		return last.ptr;
	}

	size_type _erase(const std::pair<_NodePointer, _NodePointer> where) noexcept {
		// Erase range [where.first, where.second)
		const const_iterator first(where.first);
		const const_iterator last(where.second);
		const auto count = static_cast<size_type>(std::distance(first, last));
		this->_erase(first, last);
		return count;
	}

	template<class KeyT>
	[[nodiscard]] _NodePointer _find(const KeyT& key) const {
		// Find the first element equivalent to key
		const auto result = this->_find_lower_bound(key);
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

template<class KeyT, class... Args>
struct _InPlaceKeyExtractorBase {
	// By default we can't extract the key in the emplace family and must construct a node we might not use
	static constexpr bool isExtractable = false;
};

template<class KeyT>
struct _InPlaceKeyExtractorBase<KeyT, KeyT> {
	static constexpr bool isExtractable = true;

	static const KeyT& extract(const KeyT& key) noexcept {
		return key;
	}
};

template<class... Args>
using _InPlaceKeyExtractor = _InPlaceKeyExtractorBase<std::remove_cvref_t<Args>...>;

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

	template<class... Args>
	using in_place_key_extractor = _InPlaceKeyExtractor<key_type, Args...>;

	static const key_type& key_from_node(const value_type& val) {
		return val;
	}
};

template<class T, class Comp = std::less<>>
using AVLTree = _AVLTree<_TreeTraits<T, T, Comp, false>>;

template<class T, class Comp = std::less<>>
using AVLMultiTree = _AVLTree<_TreeTraits<T, T, Comp, true>>;
#endif // ALV_TREE_H