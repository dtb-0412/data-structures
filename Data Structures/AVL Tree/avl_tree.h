#pragma once
#ifndef ALV_TREE_H
#define ALV_TREE_H

#include<iostream>
#include<functional>
#include<queue>
#include<stdexcept>

#include"node_handle.h"
#include"type_traits.hpp"

enum TreeOrder {
	PRE_ORDER, IN_ORDER, POST_ORDER, LEVEL_ORDER
};

template<class _AVLTreeVal>
class _AVLTreeConstIterator {
private:
	using _NodeType		= typename _AVLTreeVal::NodeType;
	using _NodePointer	= typename _AVLTreeVal::NodePointer;

public:
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename _AVLTreeVal::ValueType;
	using difference_type	= typename _AVLTreeVal::DifferenceType;
	using pointer			= typename _AVLTreeVal::ConstPointer;
	using reference			= const value_type&;

	_AVLTreeConstIterator() noexcept
		: ptr() {}

	_AVLTreeConstIterator(const _NodePointer ptr) noexcept
		: ptr(ptr) {}

	[[nodiscard]] _NodePointer getPointer() const noexcept {
		return ptr;
	}

	[[nodiscard]] reference operator*() const noexcept {
		return ptr->value; // UB: nullptr or end() dereference
	}

	[[nodiscard]] pointer operator->() const noexcept {
		return static_cast<pointer>(std::addressof(**this)); // UB: nullptr or end() dereference
	}

	_AVLTreeConstIterator& operator++() noexcept {
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
			ptr = _AVLTreeVal::min(ptr->right);
		}
		return *this;
	}

	_AVLTreeConstIterator& operator++(int) noexcept {
		_AVLTreeConstIterator temp = *this;
		++(*this);
		return temp;
	}

	_AVLTreeConstIterator& operator--() noexcept {
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
			ptr = _AVLTreeVal::max(ptr->left);
		}
		return *this;
	}

	_AVLTreeConstIterator& operator--(int) noexcept {
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

template<class _AVLTreeVal>
class _AVLTreeIterator : public _AVLTreeConstIterator<_AVLTreeVal> {
private:
	using _BaseIter	= _AVLTreeConstIterator<_AVLTreeVal>;
	using _BaseIter::_BaseIter;

public:
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename _AVLTreeVal::ValueType;
	using difference_type	= typename _AVLTreeVal::DifferenceType;
	using pointer			= typename _AVLTreeVal::Pointer;
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

	_AVLTreeIterator& operator++(int) noexcept {
		_AVLTreeIterator temp = *this;
		_BaseIter::operator++();
		return temp;
	}

	_AVLTreeIterator& operator--() noexcept {
		_BaseIter::operator--();
		return *this;
	}

	_AVLTreeIterator& operator--(int) noexcept {
		_AVLTreeIterator temp = *this;
		_BaseIter::operator--();
		return temp;
	}
};

template<class _ValueType>
struct _AVLTreeNode {
	using NodePointer	= _AVLTreeNode*;
	using ValueType		= _ValueType;
	using HeightType	= uint8_t;
	using BalanceType	= int8_t;

	_AVLTreeNode() = default;

	_AVLTreeNode(const _AVLTreeNode&)				= delete;
	_AVLTreeNode& operator=(const _AVLTreeNode&)	= delete;

	[[nodiscard]] static NodePointer constructHead() {
		// Construct empty head node, no value
		const NodePointer newHead = static_cast<NodePointer>(memory::allocate(1, sizeof(_AVLTreeNode)));
		memory::constructInPlace(newHead->left, newHead);
		memory::constructInPlace(newHead->right, newHead);
		newHead->parent = nullptr;
		newHead->isHead = true;
		return newHead;
	}

	template<class... Args>
	[[nodiscard]] static NodePointer constructNode(Args&&... args) {
		// Construct node from args
		const NodePointer newNode = static_cast<NodePointer>(memory::allocate(1, sizeof(_AVLTreeNode)));
		memory::constructInPlace(newNode->value, std::forward<Args>(args)...);
		newNode->left	= nullptr;
		newNode->right	= nullptr;
		newNode->parent = nullptr;
		newNode->height = 1;
		newNode->isHead = false;
		return newNode;
	}

	static void freeEmptyNode(NodePointer node) noexcept {
		// Destroy pointer members and deallocate node memory. Only empty nodes should be passed (after destroying value members in freeNode() or head node)
		memory::destructInPlace(node->left);
		memory::destructInPlace(node->right);
		memory::destructInPlace(node->parent);
		memory::deallocate(node, 1);
	}

	static void freeNode(NodePointer node) noexcept {
		// Destroy entire node, along with its value
		memory::destructInPlace(node->value);
		memory::destructInPlace(node->height);
		memory::destructInPlace(node->isHead);
		freeEmptyNode(node);
	}

	void releaseChild(NodePointer child) noexcept {
		// Release child, unlink it from *this
		this->replaceChild(child, nullptr);
	}

	void replaceChild(NodePointer oldChild, NodePointer newChild) noexcept {
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

template<class _NodeType>
struct _AVLTreeTempNode {
	// Struct to temporarily store a constructed node
	using NodeType		= _NodeType;
	using NodePointer	= typename NodeType::NodePointer;
	using ValueType		= typename NodeType::ValueType;


	template<class... Args>
	explicit _AVLTreeTempNode(Args&&... args)
		: ptr(nullptr) { // Prevent double delete when allocation throws
		ptr = NodeType::constructNode(std::forward<Args>(args)...);
	}

	_AVLTreeTempNode(const _AVLTreeTempNode&)				= delete;
	_AVLTreeTempNode& operator=(const _AVLTreeTempNode&)	= delete;

	~_AVLTreeTempNode() noexcept {
		if (ptr) {
			NodeType::freeNode(this->release());
		}
	}

	[[nodiscard]] NodePointer release() noexcept {
		// Give up node ownership and return contained pointer
		return std::exchange(ptr, nullptr);
	}

	[[nodiscard]] const ValueType& getValue() noexcept {
		return ptr->value;
	}

	NodePointer ptr;
};

enum _NodeChild : uint8_t {
	LEFT, RIGHT
};

template<class _NodePointer>
struct _NodeLocation {
	_NodePointer parent;	// Parent node under which new node will be inserted
	_NodeChild child;		// Whether to insert as left or right child
};

template<class _NodePointer>
struct _NodeFindResult {
	_NodePointer bound;						// Lower bound of the find result, used for duplicate checking
	_NodeLocation<_NodePointer> location;	// Location to insert new node
};

template<class _NodePointer>
struct _NodeFindHintResult {
	_NodeLocation<_NodePointer> location; // Location to insert new node
	bool isDuplicate;
};

template<class _ValueType, class _SizeType, class _DifferenceType, class _Pointer, class _ConstPointer, class _NodeType>
class _AVLTreeValue {
public:
	using NodeType		= _NodeType;
	using NodePointer	= typename NodeType::NodePointer;
	using HeightType	= typename NodeType::HeightType;
	using BalanceType	= typename NodeType::BalanceType;

	using ValueType			= _ValueType;
	using SizeType			= _SizeType;
	using DifferenceType	= _DifferenceType;
	using Pointer			= _Pointer;
	using ConstPointer		= _ConstPointer;
	using Reference			= ValueType&;
	using ConstReference	= const ValueType&;

	_AVLTreeValue() noexcept
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

	[[nodiscard]] static HeightType getHeight(const NodePointer node) noexcept {
		// Get node height
		return static_cast<HeightType>(node ? node->height : 0);
	}

	[[nodiscard]] static BalanceType getBalanceFactor(const NodePointer node) noexcept {
		// Get balance factor at node
		if (node) {
			const auto leftHeight	= _AVLTreeValue::getHeight(node->left);
			const auto rightHeight	= _AVLTreeValue::getHeight(node->right);
			return static_cast<BalanceType>(rightHeight - leftHeight);
		}
		return 0;
	}

	static void updateHeight(const NodePointer node) noexcept {
		// Update node height
		const auto leftHeight	= _AVLTreeValue::getHeight(node->left);
		const auto rightHeight	= _AVLTreeValue::getHeight(node->right);
		node->height = static_cast<HeightType>(std::max(leftHeight, rightHeight) + 1);
	}

	void rotateLeft(const NodePointer oldRoot) noexcept {
		// Perform counter-clockwise rotation on subtree at oldRoot
		const NodePointer parent	= oldRoot->parent;
		const NodePointer newRoot	= oldRoot->right;
		const NodePointer child		= newRoot->left;

		parent->replaceChild(oldRoot, newRoot);
		oldRoot->parent = newRoot;
		oldRoot->right	= child;
		newRoot->left	= oldRoot;
		
		if (child) { // Reattach newRoot's left child to oldRoot
			child->parent = oldRoot;
		}

		_AVLTreeValue::updateHeight(oldRoot);
		_AVLTreeValue::updateHeight(newRoot);
	}

	void rotateRight(const NodePointer oldRoot) noexcept {
		// Perform clockwise rotation on subtree at oldRoot
		const NodePointer parent	= oldRoot->parent;
		const NodePointer newRoot	= oldRoot->left;
		const NodePointer child		= newRoot->right;

		parent->replaceChild(oldRoot, newRoot);
		oldRoot->parent = newRoot;
		oldRoot->left	= child;
		newRoot->right	= oldRoot;
		
		if (child) { // Reattach newRoot's right child to oldRoot
			child->parent = oldRoot;
		}

		_AVLTreeValue::updateHeight(oldRoot);
		_AVLTreeValue::updateHeight(newRoot);
	}

	bool tryRebalance(const NodePointer node) noexcept {
		// Check for imbalance and rotate if needed
		const auto nodeBalance = _AVLTreeValue::getBalanceFactor(node);
		if (nodeBalance < -1) { // Subtree at node is imbalance to the left
			const auto leftBalance = _AVLTreeValue::getBalanceFactor(node->left);
			if (leftBalance <= 0) { // Left - Left
				this->rotateRight(node);
				return true;
			}
			// Left - Right
			this->rotateLeft(node->left);
			this->rotateRight(node);
			return true;
		}

		if (nodeBalance > 1) { // Subtree at node is imbalance to the right
			const auto rightBalance = _AVLTreeValue::getBalanceFactor(node->right);
			if (rightBalance >= 0) { // Right - Right
				this->rotateLeft(node);
				return true;
			}
			// Right - Left
			this->rotateRight(node->right);
			this->rotateLeft(node);
			return true;
		}
		return false;
	}

	void fixTree(NodePointer node, NodePointer newNode) noexcept {
		// Travel upwards from node to root, update node height and rebalance if needed
		_AVLTreeValue::updateHeight(newNode); // Reset node height for correct rebalancing
		
		while (true) {
			if (node == head) { // Reach head before rebalancing
				return;
			}

			_AVLTreeValue::updateHeight(node);
			if (this->tryRebalance(node)) { // Rebalance, stop trying
				break;
			}
			node = node->parent;
		}

		while ((node = node->parent) != head) { // Update the remaining nodes height
			_AVLTreeValue::updateHeight(node);
		}
	}

	NodePointer insert(const _NodeLocation<NodePointer> location, const NodePointer newNode) noexcept {
		// Insert newNode at location
		++size;
		if (!location.parent) { // First node in tree
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

		this->fixTree(location.parent, newNode);
		return newNode;
	}

	std::pair<NodePointer, NodePointer> extract(const _AVLTreeConstIterator<_AVLTreeValue> pos) noexcept {
		// Extract node pointed by pos
		--size;
		const NodePointer extracted = pos.getPointer(); // UB: pos == AVLTree::end()
		const NodePointer nextNode	= std::next(pos, 1).getPointer();
		if (size == 0) { // Extract final node
			head->left	= nullptr;
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
			successor->parent->releaseChild(successor);
			extracted->parent->replaceChild(extracted, successor);

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
			parent->releaseChild(extracted);
		}
		else { // Node has a single child
			const NodePointer childNode = std::exchange((extracted->left) ? extracted->left : extracted->right, nullptr);
			parent->replaceChild(extracted, childNode);
		}

		this->fixTree(std::exchange(extracted->parent, nullptr), extracted);
		return std::make_pair(extracted, nextNode);
	}

	void clear(NodePointer node) noexcept {
		// Clear entire subtree at node recursively
		while (node) {
			this->clear(node->right);
			NodeType::freeNode(std::exchange(node, node->left));
		}
	}

	/*
		Serve as the end() node for tree traversal

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
	using value_type		= T;
	using size_type			= std::size_t;
	using difference_type	= std::ptrdiff_t;
	using pointer			= T*;
	using const_pointer		= const T*;
	using reference			= T&;
	using const_reference	= const T&;

private:
	using _NodeType		= _AVLTreeNode<value_type>;
	using _NodePointer	= typename _NodeType::NodePointer;

	using _AVLTreeVal	= _AVLTreeValue<value_type, size_type, difference_type, pointer, const_pointer, _NodeType>;

	enum _CopyStrategy {
		COPY, MOVE
	};

public:
	using iterator			= _AVLTreeConstIterator<_AVLTreeVal>;
	using const_iterator	= _AVLTreeConstIterator<_AVLTreeVal>;

public:
	AVLTree()
		: _data() {
		// Construct empty tree
		_data.head = _NodeType::constructHead();
	}

	AVLTree(const AVLTree& other)
		: _data() {
		this->_copy(other, _CopyStrategy::COPY);
	}

	~AVLTree() noexcept {
		_data.clear(_data.head->parent);
		_NodeType::freeEmptyNode(_data.head);
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

	[[nodiscard]] size_type maxSize() const noexcept {
		return static_cast<size_type>(-1) / sizeof(_NodeType);
	}

	[[nodiscard]] bool isEmpty() const noexcept {
		return _data.size == 0;
	}

	template<class... Args>
	std::pair<iterator, bool> emplace(Args&&... args) {
		// Insert by constructing in place using args
		const auto result = this->_emplace(std::forward<Args>(args)...);
		return std::make_pair(iterator(result.first), result.second);
	}

	template<class... Args>
	iterator emplaceHint(const_iterator hint, Args&&... args) {
		// Insert with hint by constructing in place using args
		return iterator(this->_emplaceHint(hint.getPointer(), std::forward<Args>(args)...));
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
		return this->emplaceHint(hint, val);
	}

	iterator insert(const_iterator hint, value_type&& val) {
		// Insert with hint by moving val
		return this->emplaceHint(hint, std::move(val));
	}

	template<class InputIter,
		std::enable_if_t<traits::IsInputIter<InputIter>, bool> = true>
	void insert(InputIter first, const InputIter last) {
		// Insert range [first, last)
		for (; first != last; ++first) {
			this->_emplaceHint(_data.head, *first);
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
		const auto result = this->_findLowerBound(key);
		if (!this->_isDuplicateKey(result.bound, key)) { // Key does not exist
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
	template<class KeyType, class Compare = Comp,
		std::enable_if_t<
			traits::IsTransparent<Comp> &&
			!std::is_convertible_v<KeyType, iterator> &&
			!std::is_convertible_v<KeyType, const_iterator>, int> = 0>
	bool erase(const KeyType& key) noexcept {
		// Erase key
		const auto result = this->_findLowerBound(key);
		if (!this->_isDuplicateKey(result.bound, key)) { // Key does not exist
			return false;
		}
		this->_erase(const_iterator(result.bound));
		return true;
	}

	void clear() noexcept {
		// Erase all
		_data.clear(std::exchange(_data.head->parent, nullptr));
		_data.head->left	= _data.head;
		_data.head->right	= _data.head;
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

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] iterator find(const KeyType& key) {
		// Find element equivalent to key
		return iterator(this->_find(key));
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] const_iterator find(const KeyType& key) const {
		// Find element equivalent to key
		return const_iterator(this->_find(key));
	}

	[[nodiscard]] bool contains(const value_type& key) const {
		// Check if tree contains key
		return this->_isDuplicateKey(this->_findLowerBound(key).bound, key);
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] bool contains(const KeyType& key) const {
		// Check if tree contains element equivalent to key
		return this->_isDuplicateKey(this->_findLowerBound(key).bound, key);
	}

	[[nodiscard]] size_type count(const value_type& key) const {
		// Count occurrences of key
		return this->_isDuplicateKey(this->_findLowerBound(key).bound, key);
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] size_type count(const KeyType& key) const {
		// Count occurrences of value equivalent to key
		return this->_isDuplicateKey(this->_findLowerBound(key).bound, key);
	}
	
	[[nodiscard]] iterator lowerBound(const value_type& key) {
		// Find the first element not less than key
		return iterator(this->_findLowerBound(key).bound);
	}

	[[nodiscard]] const_iterator lowerBound(const value_type& key) const {
		// Find the first element not less than key
		return const_iterator(this->_findLowerBound(key).bound);
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] iterator lowerBound(const KeyType& key) {
		// Find the first equivalent element not less than key
		return iterator(this->_findLowerBound(key).bound);
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] const_iterator lowerBound(const KeyType& key) const {
		// Find the first equivalent element not less than key
		return const_iterator(this->_findLowerBound(key).bound);
	}

	[[nodiscard]] iterator upperBound(const value_type& key) {
		// Find the first element greater than key
		return iterator(this->_findUpperBound(key).bound);
	}

	[[nodiscard]] const_iterator upperBound(const value_type& key) const {
		// Find the first element greater than key
		return const_iterator(this->_findUpperBound(key).bound);
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] iterator upperBound(const KeyType& key) {
		// Find the first equivalent element greater than key
		return iterator(this->_findUpperBound(key).bound);
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] const_iterator upperBound(const KeyType& key) const {
		// Find the first equivalent element greater than key
		return const_iterator(this->_findUpperBound(key).bound);
	}

	template<class, class>
	friend class AVLTree;

	template<class OtherCompare>
	void merge(AVLTree<T, OtherCompare>& other) {
		// Merge other into *this, leaving other empty
		if constexpr (std::is_same_v<AVLTree, AVLTree<T, OtherCompare>>) {
			if (this == std::addressof(other)) {
				return;
			}
		}

		for (auto iter = other.begin(); iter != other.end();) {
			const auto currNode = iter.ptr;
			++iter; // Important: increment iterator before extraction
			
			_NodeFindResult<_NodePointer> result = this->_findLowerBound(currNode->value);
			if (this->_isDuplicateKey(result.bound, currNode->value)) {
				continue;
			}

			this->_checkMaxSize();
			// Extract from other and reset links
			const auto extracted	= other._data.extract(const_iterator(currNode)).first;
			extracted->left			= nullptr;
			extracted->right		= nullptr;
			// Insert back into *this
			_data.insert(result.location, extracted); // Handle extracted->parent and extracted->height
		}
	}

	template<class OtherCompare>
	void merge(AVLTree<T, OtherCompare>&& other) {
		// Merge other into *this, leaving other empty
		this->merge(other);
	}

#if _MSVC_LANG >= 201703L
	using NodeHandle = _NodeHandle<_NodeType, _NodeHandleSetBase, value_type>;

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
		_NodeFindResult<_NodePointer> result = this->_findLowerBound(node->value);
		if (this->_isDuplicateKey(result.bound, node->value)) {
			return InsertReturnType<iterator, NodeHandle>{iterator(result.bound), false, std::move(handle)};
		}

		this->_checkMaxSize();

		node->left	= nullptr;
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
		_NodeFindHintResult<_NodePointer> result = this->_findHint(hint.getPointer(), node->value);
		if (result.isDuplicate) {
			return iterator(result.location.parent);
		}

		this->_checkMaxSize();

		node->left	= nullptr;
		node->right = nullptr;
		const auto inserted = _data.insert(result.location, handle._release());
		return iterator(inserted);
	}
#endif // Has C++17

	struct DefaultPrint {
		// Default print functor
		template<class NodePointer>
		void operator()(NodePointer node) const noexcept {
			std::cout << node->value << " ";
		}
	};

	template<class PrintFnc = DefaultPrint>
	void print(const TreeOrder order, PrintFnc print = PrintFnc{}) {
		// Print tree in specified order using print function
		const _NodePointer root = _data.head->parent;
		if (!root) {
			std::cout << "Empty!\n";
			return;
		}

		switch (order) {
			case PRE_ORDER: {
				this->_preOrder(root, print);
				break;
			}
			case IN_ORDER: {
				this->_inOrder(root, print);
				break;
			}
			case POST_ORDER: {
				this->_postOrder(root, print);
				break;
			}
			case LEVEL_ORDER: {
				this->_levelOrder(root, print);
				break;
			}
		}
		std::cout << "\n";
	}

private:
	_NodePointer _copyNode(value_type& val, _CopyStrategy strat) {
		// Construct node by copying or moving val, depending on strat
		if (strat == _CopyStrategy::COPY) {
			return _NodeType::constructNode(val);
		}
		return _NodeType::constructNode(std::move(val));
	}

	_NodePointer _copySubtree(_NodePointer oldRoot, _NodePointer newHead, _CopyStrategy strat) {
		// Copy or move subtree at oldRoot into newHead recursively, depending on strat
		_NodePointer newRoot = newHead->parent;
		if (!oldRoot->isHead) { // Copy or move a node, then recurse into its subtree
			newRoot			= this->_copyNode(oldRoot->value, strat); // Copy and memorize new root
			newRoot->parent = newHead;
			newRoot->height = oldRoot->height;
			newRoot->left	= this->_copySubtree(oldRoot->left, newRoot, strat);
			newRoot->right	= this->_copySubtree(oldRoot->right, newRoot, strat);
		}
		else {
			std::cout << "Copy from empty tree!\n";
		}
		return newRoot;
	}

	void _copy(const AVLTree& other, _CopyStrategy strat) {
		// Copy or move entire tree from other, depending on strat
		_data.head->parent = this->_copySubtree(other._data.head->parent, _data.head, strat);
		// Update leftmost and rightmost nodes
		if ((_data.size = other._data.size) == 0) {
			_data.head->left	= _data.head;
			_data.head->right	= _data.head;
		}
		else {
			_data.head->left	= _AVLTreeVal::min(_data.head->parent);
			_data.head->right	= _AVLTreeVal::max(_data.head->parent);
		}
	}

	template<class KeyType>
	[[nodiscard]] bool _isDuplicateKey(const _NodePointer bound, const KeyType& key) const {
		// Check if key is duplicate by comparing with bound
		return !bound->isHead && !(_comp(key, bound->value));
	}

	template<class KeyType>
	[[nodiscard]] _NodeFindResult<_NodePointer> _findLowerBound(const KeyType& key) const {
		/*
			Find the smallest (or leftmost in-order) node that is not less than key (or does not satisfy _comp(node value, key))
			
			Traverse the whole path downwards from root until nullptr is reached.
			At each node, perform exactly 01 comparison using _comp::operator() on key and node value.

			Let N be the total number of nodes:
			Best case:		O(1),		using 01 comparison (root case)
			Worst case:		O(log2(N)), using log2(N) comparisons
			Average case:	O(log2(N))
		*/
		_NodeFindResult<_NodePointer> result{ _data.head, { _data.head->parent, _NodeChild::RIGHT } };
		for (_NodePointer currNode = result.location.parent; currNode;) {
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

	template<class KeyType>
	[[nodiscard]] _NodeFindResult<_NodePointer> _findUpperBound(const KeyType& key) const {
		// Find the smallest (or leftmost in-order) node that is strictly greater than key (or satisfies _comp(key, node value))
		_NodeFindResult<_NodePointer> result{ _data.head, { _data.head->parent, _NodeChild::RIGHT } };
		for (_NodePointer currNode = result.location.parent; currNode;) {
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

	template<class KeyType>
	[[nodiscard]] _NodeFindHintResult<_NodePointer> _findHint(const _NodePointer hintNode, const KeyType& key) const {
		// Find node insert location using hintNode
		const _NodePointer head = _data.head;
		if (hintNode == head->left) { // Insert at begin as leftmost node
			if (_comp(key, hintNode->value)) {
				return { { hintNode, _NodeChild::LEFT }, false };
			}
		}
		else if (hintNode->isHead) { // Insert at end as rightmost node
			if (!head->parent || _comp(head->right->value, key)) {
				return { { head->right, _NodeChild::RIGHT }, false };
			}
		}
		else if (_comp(key, hintNode->value)) { // key < *hintNode
			const _NodePointer prevNode = std::prev(const_iterator(hintNode), 1).getPointer();
			if (_comp(prevNode->value, key)) { // *(--hintNode) < key < *hintNode, insert here
				if (!prevNode->right) {
					return { { prevNode, _NodeChild::RIGHT }, false };
				}
				return { { hintNode, _NodeChild::LEFT }, false };
			}
		}
		else if (_comp(hintNode->value, key)) { // key > *hintNode
			const _NodePointer nextNode = std::next(const_iterator(hintNode), 1).getPointer();
			if (nextNode->isHead || _comp(key, nextNode->value)) { // *hintNode < key < *(++hintNode), insert here
				if (!hintNode->right) {
					return { { hintNode, _NodeChild::RIGHT }, false };
				}
				return { { nextNode, _NodeChild::LEFT }, false };
			}
		}
		else { // Duplicate value, don't insert
			return { { hintNode, _NodeChild::LEFT, }, true };
		}
		// Incorrect hint, key is not in the proximity of *hintNode. Resort to the usual find method
		const auto result = this->_findLowerBound(key);
		if (this->_isDuplicateKey(result.bound, key)) {
			return { { result.bound, _NodeChild::LEFT }, true };
		}
		return { result.location, false };
	}

	template<class... Args>
	std::pair<_NodePointer, bool> _emplace(Args&&... args) {
		// Insert by constructing node inplace using args
		_AVLTreeTempNode<_NodeType> tempNode(std::forward<Args>(args)...); // Create temporary node for initial node search
		const auto& key = tempNode.getValue();

		const auto result = this->_findLowerBound(key); // Find insert location
		if (this->_isDuplicateKey(result.bound, key)) { // Duplicate check
			return std::make_pair(result.bound, false);
		}

		this->_checkMaxSize();

		const _NodePointer newNode = tempNode.release(); // Safe to insert, release temp node, transfer ownership to *this
		return std::make_pair(_data.insert(result.location, newNode), true);
	}

	template<class... Args>
	_NodePointer _emplaceHint(const _NodePointer hintNode, Args&&... args) {
		// Insert by constructing node inplace using args with given hint
		_AVLTreeTempNode<_NodeType> tempNode(std::forward<Args>(args)...);
		const auto& key = tempNode.getValue();

		const auto result = this->_findHint(hintNode, key);
		if (result.isDuplicate) {
			return result.location.parent;
		}

		this->_checkMaxSize();

		const _NodePointer newNode = tempNode.release();
		return _data.insert(result.location, newNode);
	}

	_NodePointer _erase(const_iterator pos) noexcept {
		// Erase node at pos, return the next in-order node
		const auto result = _data.extract(pos); // UB
		_NodeType::freeNode(result.first);
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

	template<class KeyType>
	[[nodiscard]] _NodePointer _find(const KeyType& key) const {
		// Find element equivalent to key
		const auto result = this->_findLowerBound(key);
		if (this->_isDuplicateKey(result.bound, key)) {
			return result.bound;
		}
		return _data.head;
	}

	void _checkMaxSize() {
		// Check if tree has reached max size
		if (this->maxSize() == _data.size) {
			throw std::length_error("container reached max size");
		}
	}

	template<class PrintFnc>
	void _preOrder(_NodePointer node, PrintFnc print) {
		// Print subtree at node in pre-order
		if (!node) {
			return;
		}

		print(node);
		this->_preOrder(node->left, print);
		this->_preOrder(node->right, print);
	}

	template<class PrintFnc>
	void _inOrder(_NodePointer node, PrintFnc print) {
		// Print subtree at node in in-order
		if (!node) {
			return;
		}

		this->_inOrder(node->left, print);
		print(node);
		this->_inOrder(node->right, print);
	}

	template<class PrintFnc>
	void _postOrder(_NodePointer node, PrintFnc print) {
		// Print subtree at node in post-order
		if (!node) {
			return;
		}

		this->_postOrder(node->left, print);
		this->_postOrder(node->right, print);
		print(node);
	}

	template<class PrintFnc>
	void _levelOrder(_NodePointer root, PrintFnc print) {
		// Print subtree at node in level-order
		std::queue<_NodePointer> nodesQueue;
		nodesQueue.push(root);
		while (!nodesQueue.empty()) {
			const _NodePointer node = nodesQueue.front();
			print(node);

			nodesQueue.pop();
			if (node->left) {
				nodesQueue.push(node->left);
			}
			if (node->right) {
				nodesQueue.push(node->right);
			}
		}
	}

private:
	_AVLTreeVal _data;
	Comp		_comp;
};
#endif // ALV_TREE_H