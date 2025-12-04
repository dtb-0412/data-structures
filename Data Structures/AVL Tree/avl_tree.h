#pragma once
#ifndef ALV_TREE_H
#define ALV_TREE_H

#include<queue>
#include<iostream>
#include<functional>

#include"node_handle.h"
#include"memory.hpp"
#include"type_traits.hpp"

using std::cout;

enum Traversal {
	PRE, IN, POST, LEVEL, LEVEL_H
};

template<class AVLTreeVal>
class _AVLTreeConstIterator {
private:
	using _Node			= typename AVLTreeVal::Node;
	using _NodePointer	= typename AVLTreeVal::NodePointer;

public:
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type		= typename AVLTreeVal::value_type;
	using difference_type	= typename AVLTreeVal::difference_type;
	using pointer			= typename AVLTreeVal::const_pointer;
	using reference			= const value_type&;

	_AVLTreeConstIterator() noexcept
		: _ptr() {}

	_AVLTreeConstIterator(const _NodePointer ptr) noexcept
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

	_AVLTreeConstIterator& operator++() noexcept {
		if (!_ptr->right) { // Climb upwards, look for right subtree
			for (_NodePointer currNode = _ptr->parent;;) {
				// Stop when reaching head or going upwards from a left subtree for the first time
				if (currNode->isHead || _ptr != currNode->right) {
					_ptr = currNode; // Goes from the rightmost node to head for end()
					break;
				}
				_ptr = std::exchange(currNode, currNode->parent);
			}
		}
		else { // Goes to the leftmost node of right subtree
			_ptr = AVLTreeVal::min(_ptr->right);
		}
		return *this;
	}

	_AVLTreeConstIterator& operator++(int) noexcept {
		_AVLTreeConstIterator temp = *this;
		++(*this);
		return temp;
	}

	_AVLTreeConstIterator& operator--() noexcept {
		if (_ptr->isHead) { // Goes back from end() to the rightmost node
			_ptr = _ptr->right;
		}
		else if (!_ptr->left) {
			for (_NodePointer currNode = _ptr->parent;;) {
				// Stop when reaching head or going upwards from a right subtree for the first time
				if (currNode->isHead || _ptr != currNode->left) {
					if (!_ptr->isHead) {
						_ptr = currNode;
					}
					break;
				}
				_ptr = std::exchange(currNode, currNode->parent);
			}
		}
		else { // Goes to the rightmost node of left subtree
			_ptr = AVLTreeVal::max(_ptr->left);
		}
		return *this;
	}

	_AVLTreeConstIterator& operator--(int) noexcept {
		_AVLTreeConstIterator temp = *this;
		--(*this);
		return temp;
	}

	[[nodiscard]] bool operator==(const _AVLTreeConstIterator& other) const noexcept {
		return _ptr == other._ptr;
	}

	[[nodiscard]] bool operator!=(const _AVLTreeConstIterator& other) const noexcept {
		return !(*this == other);
	}

private:
	_NodePointer _ptr;
};


template<class AVLTreeVal>
class _AVLTreeIterator : public _AVLTreeConstIterator<AVLTreeVal> {
private:
	using _BaseIter	= _AVLTreeConstIterator<AVLTreeVal>;
	using _BaseIter::_BaseIter;

public:
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


template<class ValueType>
struct _AVLTreeNode {
	using NodePointer	= _AVLTreeNode*;

	using value_type	= ValueType;
	using height_type	= uint8_t;
	using balance_type	= int8_t;

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

	value_type	value;	// sizeof(value_type)
	height_type	height; // 1 byte, assuming AVL tree height <= 255
	
	bool isHead;		// 1 byte boolean
};

template<class NodeType>
struct _AVLTreeTempNode {
	// Struct to temporarily store a constructed node
	using Node			= NodeType;
	using NodePointer	= typename Node::NodePointer;

	using value_type	= typename Node::value_type;


	template<class... Args>
	explicit _AVLTreeTempNode(Args&&... args)
		: node(nullptr) { // Prevent double delete when allocation throws
		node = Node::constructNode(std::forward<Args>(args)...);
	}

	_AVLTreeTempNode(const _AVLTreeTempNode&)				= delete;
	_AVLTreeTempNode& operator=(const _AVLTreeTempNode&)	= delete;

	~_AVLTreeTempNode() noexcept {
		if (node) {
			Node::freeNode(this->release());
		}
	}

	[[nodiscard]] NodePointer release() noexcept {
		// Give up node ownership and return contained pointer
		return std::exchange(node, nullptr);
	}

	[[nodiscard]] const value_type& getValue() noexcept {
		return node->value;
	}

	NodePointer node;
};

enum _NodeChild : uint8_t {
	LEFT, RIGHT
};

template<class NodePointer>
struct _NodeLocation {
	NodePointer	parent; // Parent node under which new node will be inserted
	_NodeChild	child;	// Whether to insert as left or right child
};

template<class NodePointer>
struct _NodeFindResult {
	NodePointer	bound;						// Lower bound of the find result, used for duplicate checking
	_NodeLocation<NodePointer> location;	// Location to insert new node
};

template<class NodePointer>
struct _NodeFindHintResult {
	_NodeLocation<NodePointer> location; // Location to insert new node
	bool isDuplicate;
};

template<class ValueType, class SizeType, class DiffType, class Pointer, class ConstPointer, class NodeType>
class _AVLTreeValue {
public:
	using Node			= NodeType;
	using NodePointer	= typename Node::NodePointer;

	using height_type	= typename Node::height_type;
	using balance_type	= typename Node::balance_type;

	using value_type		= ValueType;
	using size_type			= SizeType;
	using difference_type	= DiffType;
	using pointer			= Pointer;
	using const_pointer		= ConstPointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

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

	[[nodiscard]] static height_type getHeight(const NodePointer node) noexcept {
		// Get node height
		return static_cast<height_type>(node ? node->height : 0);
	}

	[[nodiscard]] static balance_type getBalanceFactor(const NodePointer node) noexcept {
		// Get balance factor at node
		if (node) {
			const auto leftHeight	= _AVLTreeValue::getHeight(node->left);
			const auto rightHeight	= _AVLTreeValue::getHeight(node->right);
			return static_cast<balance_type>(rightHeight - leftHeight);
		}
		return 0;
	}

	static void updateHeight(const NodePointer node) noexcept {
		// Update node height
		const auto leftHeight	= _AVLTreeValue::getHeight(node->left);
		const auto rightHeight	= _AVLTreeValue::getHeight(node->right);
		node->height = static_cast<height_type>(std::max(leftHeight, rightHeight) + 1);
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

	void fixTree(NodePointer node) noexcept {
		// Travel upwards from node to root, update node height and rebalance if needed
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

		this->fixTree(location.parent);
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

		this->fixTree(std::exchange(extracted->parent, nullptr));
		return std::make_pair(extracted, nextNode);
	}

	void clear(NodePointer node) noexcept {
		// Clear entire subtree at node recursively
		while (node) {
			this->clear(node->right);
			Node::freeNode(std::exchange(node, node->left));
		}
	}

	NodePointer head;
	/*
		Serve as the end() node for tree traversal

		head->left:		points to the leftmost node (min node)
		head->right:	points to the rightmost node (max node)
		head->parent:	points to the actual root node
	*/
	size_type size; // Total number of nodes
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
	using _Node			= _AVLTreeNode<value_type>;
	using _NodePointer	= typename _Node::NodePointer;

	using _AVLTreeVal	= _AVLTreeValue<value_type, size_type, difference_type, pointer, const_pointer, _Node>;

	enum _CopyStrategy {
		COPY, MOVE
	};

public:
	using Iterator		= _AVLTreeConstIterator<_AVLTreeVal>;
	using ConstIterator = _AVLTreeConstIterator<_AVLTreeVal>;

public:
	AVLTree()
		: _data() {
		// Construct empty tree
		_data.head = _Node::constructHead();
	}

	AVLTree(const AVLTree& other)
		: _data() {
		this->_copy(other, _CopyStrategy::COPY);
	}

	~AVLTree() noexcept {
		_data.clear(_data.head->parent);
		_Node::freeEmptyNode(_data.head);
	}

	[[nodiscard]] Iterator begin() noexcept {
		return Iterator(_data.head->left);
	}

	[[nodiscard]] ConstIterator begin() const noexcept {
		return ConstIterator(_data.head->left);
	}

	[[nodiscard]] Iterator end() noexcept {
		return Iterator(_data.head);
	}

	[[nodiscard]] ConstIterator end() const noexcept {
		return ConstIterator(_data.head);
	}

	[[nodiscard]] ConstIterator cbegin() const noexcept {
		return this->begin();
	}

	[[nodiscard]] ConstIterator cend() const noexcept {
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
		return static_cast<size_type>(-1) / sizeof(_Node);
	}

	[[nodiscard]] bool isEmpty() const noexcept {
		return _data.size == 0;
	}

	template<class... Args>
	std::pair<Iterator, bool> emplace(Args&&... args) {
		// Insert by constructing in place using args
		const auto result = this->_emplace(std::forward<Args>(args)...);
		return std::make_pair(Iterator(result.first), result.second);
	}

	template<class... Args>
	Iterator emplaceHint(ConstIterator hint, Args&&... args) {
		// Insert with hint by constructing in place using args
		return Iterator(this->_emplaceHint(hint.getPointer(), std::forward<Args>(args)...));
	}

	std::pair<Iterator, bool> insert(const value_type& val) {
		// Insert by copying val
		return this->emplace(val);
	}

	std::pair<Iterator, bool> insert(value_type&& val) {
		// Insert by moving val
		return this->emplace(std::move(val));
	}

	Iterator insert(ConstIterator hint, const value_type& val) {
		// Insert with hint by copying val
		return this->emplaceHint(hint, val);
	}

	Iterator insert(ConstIterator hint, value_type&& val) {
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
		// Insert all of initList
		this->insert(initList.begin(), initList.end());
	}

	Iterator erase(ConstIterator pos) noexcept {
		// Erase at pos
		return Iterator(this->_erase(pos)); // UB
	}

	Iterator erase(ConstIterator first, ConstIterator last) noexcept {
		// Erase range [first, last)
		const auto begin = this->begin();
		if (first == this->begin() && last == this->end()) { // Erase entire tree
			this->clear();
			return static_cast<Iterator>(last);
		}
		// Erase nodes one at a time
		while (first != last) {
			this->_erase(first++);
		}
		return static_cast<Iterator>(last);
	}

	bool erase(const value_type& key) noexcept {
		const auto result = this->_findNodeLocation(key);
		if (!this->_isDuplicateKey(result.bound, key)) { // Key does not exist
			return false;
		}
		this->_erase(ConstIterator(result.bound));
		return true;
	}

	[[nodiscard]] Iterator find(const value_type& key) {
		return Iterator(this->_find(key));
	}

	[[nodiscard]] ConstIterator find(const value_type& key) const {
		return ConstIterator(this->_find(key));
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] Iterator find(const KeyType& key) {
		return Iterator(this->_find(key));
	}

	template<class KeyType, class Compare = Comp,
		std::enable_if_t<traits::IsTransparent<Compare>, int> = 0>
	[[nodiscard]] ConstIterator find(const KeyType& key) const {
		return ConstIterator(this->_find(key));
	}

	// swap
	// extract
	// contains
	// count
	// lowerBound
	// upperBound
	// equalRange
	// merge

	void clear() noexcept {
		// Clear entire tree and reset head node
		_data.clear(std::exchange(_data.head->parent, nullptr));
		_data.head->left	= _data.head;
		_data.head->right	= _data.head;
		_data.size = 0;
	}

#if __cplusplus >= 201703L

public:
	using NodeHandle = _NodeHandle<_Node, _NodeHandleSetBase, value_type>;

	NodeHandle extract(const ConstIterator pos) {
		const auto result = _data.extract(pos);
		return NodeHandle::make(result.first);
	}

	/*NodeHandle extract(const value_type& key) {
		
	}*/
#endif // Has C++17

	// Testing purpose only
	void print(const Traversal order = Traversal::IN) {
		std::cout << "My tree: ";
		const _NodePointer root = _data.head->parent;
		if (!root) {
			std::cout << "Empty!\n";
			return;
		}

		switch (order) {
			case PRE: {
				break;
			}
			case IN: {
				this->_inOrder(root);
				break;
			}
			case POST: {
				break;
			}
			case LEVEL: {
				this->_levelOrder(root, [](const _NodePointer& node) { std::cout << node->value << " "; });
				break;
			}
			case LEVEL_H:
			{
				this->_levelOrder(root, [](const _NodePointer& node) { std::cout << static_cast<int>(node->height) << " "; });
				break;
			}
		}
		std::cout << "\nMin: " << _data.head->left->value << " - Max: " << _data.head->right->value << "\n";
		//std::cout << "Root->parent: " << _data.head->parent->parent << "\n";
	}

	void _inOrder(_NodePointer node) {
		if (!node) {
			return;
		}

		this->_inOrder(node->left);
		std::cout << node->value << " ";
		this->_inOrder(node->right);
	}

	template<class Print>
	void _levelOrder(_NodePointer root, Print printNode) {
		std::queue<_NodePointer> nodesQueue;
		nodesQueue.push(root);
		while (!nodesQueue.empty()) {
			const _NodePointer node = nodesQueue.front();
			//std::cout << node->value << " ";
			printNode(node);

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
	_NodePointer _copyNode(value_type& val, _CopyStrategy strat) {
		// Construct node by copying or moving val, depending on strat
		if (strat == _CopyStrategy::COPY) {
			return _Node::constructNode(val);
		}
		return _Node::constructNode(std::move(val));
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

	[[nodiscard]] bool _isDuplicateKey(const _NodePointer bound, const value_type& key) const {
		// Check if key is duplicate by comparing with bound
		return !bound->isHead && !(key < bound->value);
	}

	[[nodiscard]] _NodeFindResult<_NodePointer> _findNodeLocation(const value_type& key) const {
		/*
			Traverse the whole path downwards from root until nullptr is reached.
			At each node, perform only 01 comparison using operator<() between key and node value.

			Let N be the total number of nodes:
			Best case:		O(1),		using 01 comparison (root case)
			Worst case:		O(log2(N)), using log2(N) comparisons
			Average case:	O(log2(N))
		*/
		_NodeFindResult<_NodePointer> result{ _data.head, { _data.head->parent, _NodeChild::RIGHT } };
		for (_NodePointer currNode = result.location.parent; currNode;) {
			result.location.parent = currNode;
			if (currNode->value < key) {
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

	[[nodiscard]] _NodeFindHintResult<_NodePointer> _findNodeLocationHint(const _NodePointer hintNode, const value_type& key) const {
		// Find node insert location using hintNode
		const _NodePointer head = _data.head;
		if (hintNode == head->left) { // Insert at begin as leftmost node
			if (key < hintNode->value) {
				return { { hintNode, _NodeChild::LEFT }, false };
			}
		}
		else if (hintNode->isHead) { // Insert at end as rightmost node
			if (!head->parent || head->right->value < key) {
				return { { head->right, _NodeChild::RIGHT }, false };
			}
		}
		else if (key < hintNode->value) { // key < *hintNode
			const _NodePointer prevNode = std::prev(ConstIterator(hintNode), 1).getPointer();
			if (prevNode->value < key) { // *(--hintNode) < key < *hintNode, insert here
				if (!prevNode->right) {
					return { { prevNode, _NodeChild::RIGHT }, false };
				}
				return { { hintNode, _NodeChild::LEFT }, false };
			}
		}
		else if (hintNode->value < key) { // key > *hintNode
			const _NodePointer nextNode = std::next(ConstIterator(hintNode), 1).getPointer();
			if (nextNode->isHead || key < nextNode->value) { // *hintNode < key < *(++hintNode), insert here
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
		const auto result = this->_findNodeLocation(key);
		if (this->_isDuplicateKey(result.bound, key)) {
			return { { result.bound, _NodeChild::LEFT }, true };
		}
		return { result.location, false };
	}

	template<class... Args>
	std::pair<_NodePointer, bool> _emplace(Args&&... args) {
		// Insert by constructing node inplace using args
		_AVLTreeTempNode<_Node> tempNode(std::forward<Args>(args)...); // Create temporary node for initial node search
		const auto& key = tempNode.getValue();

		const auto result = this->_findNodeLocation(key); // Find insert location
		if (this->_isDuplicateKey(result.bound, key)) { // Duplicate check
			return std::make_pair(result.bound, false);
		}

		const _NodePointer newNode = tempNode.release(); // Safe to insert, release temp node, transfer ownership to *this
		return std::make_pair(_data.insert(result.location, newNode), true);
	}

	template<class... Args>
	_NodePointer _emplaceHint(const _NodePointer hintNode, Args&&... args) {
		// Insert by constructing node inplace using args with given hint
		_AVLTreeTempNode<_Node> tempNode(std::forward<Args>(args)...);
		const auto& key = tempNode.getValue();

		const auto result = this->_findNodeLocationHint(hintNode, key);
		if (result.isDuplicate) {
			return result.location.parent;
		}

		const _NodePointer newNode = tempNode.release();
		return _data.insert(result.location, newNode);
	}

	_NodePointer _erase(ConstIterator pos) noexcept {
		// Erase node at pos, return the next in-order node
		const auto result = _data.extract(pos); // UB
		_Node::freeNode(result.first);
		return result.second;
	}

	template<class T>
	[[nodiscard]] _NodePointer _find(const T& key) const {
		const auto result = this->_findNodeLocation(key);
		if (this->_isDuplicateKey(result.bound, key)) {
			return result.bound;
		}
		return _data.head;
	}

private:
	_AVLTreeVal _data;
};
#endif // ALV_TREE_H