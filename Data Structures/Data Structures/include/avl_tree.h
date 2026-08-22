#pragma once
#ifndef AVL_TREE_H
#define AVL_TREE_H

#include"tree_core.h"

template<class ValueT>
struct _AVLTreeNode : public _BSTreeNode<ValueT, _AVLTreeNode<ValueT>> {
private:
	using _BaseNode = _BSTreeNode<ValueT, _AVLTreeNode<ValueT>>;

public:
	using node_pointer	= typename _BaseNode::node_pointer;
	using value_type	= typename _BaseNode::value_type;

	using height_type	= uint8_t;

	using _BaseNode::left;
	using _BaseNode::right;
	using _BaseNode::parent;
	using _BaseNode::isNil;
	
	_AVLTreeNode() = default;

	_AVLTreeNode(const _AVLTreeNode&)				= delete;
	_AVLTreeNode& operator=(const _AVLTreeNode&)	= delete;

	[[nodiscard]] static node_pointer construct_head() {
		// Construct empty head sentinel with no value
		const auto newHead = _BaseNode::construct_head();
		newHead->height = 0;
		return newHead;
	}

	template<class... Args>
	[[nodiscard]] static node_pointer construct_node(node_pointer head, Args&&... args) {
		// Construct node with value from args
		const auto newNode = _BaseNode::construct_node(head, std::forward<Args>(args)...);
		newNode->height = 1;
		return newNode;
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

	height_type	height;	// 1 byte, assuming BinarySearch tree height <= 255
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class _AVLTreeCore : public _BSTreeCore<ValueT, SizeT, DiffT, Ptr, ConstPtr, NodeT> {
private:
	using _BaseVal = _BSTreeCore<ValueT, SizeT, DiffT, Ptr, ConstPtr, NodeT>;

public:
	using node_type		= typename _BaseVal::node_type;
	using node_pointer	= typename _BaseVal::node_pointer;

	using height_type	= typename node_type::height_type;
	using balance_type	= int8_t;

public:
	using _BaseVal::head;
	using _BaseVal::size;

	_AVLTreeCore() = default;

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
		else if (location.child == _NodeChild::RIGHT) { // Insert as right child
			location.parent->right = newNode;
			if (location.parent == head->right) { // New max node, update head->right
				head->right = newNode;
			}
		}

		this->_fix_tree(location.parent, newNode);
		return newNode;
	}

	node_pointer extract(_BSTreeConstIterator<_AVLTreeCore> where) noexcept {
		// Extract node pointed by where
		--size;
		const node_pointer extracted = where.ptr; // UB: where == _BSTree::end()
		if (size == 0) { // Extract final node
			head->left = head;
			head->right = head;
		}
		else if (extracted == head->left) { // Extract leftmost node
			head->left = (++_BSTreeConstIterator(where)).ptr;
		}
		else if (extracted == head->right) { // Extract rightmost node
			head->right = (--_BSTreeConstIterator(where)).ptr;
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

		this->_fix_tree(std::exchange(extracted->parent, head), extracted);
		return extracted;
	}

private:
	[[nodiscard]] static balance_type _get_balance_factor(node_pointer node) noexcept {
		// Get balance factor at node
		return static_cast<balance_type>(node->right->height - node->left->height);
	}

	static void _update_height(node_pointer node) noexcept {
		// Update node height
		node->height = static_cast<height_type>(std::max(node->left->height, node->right->height) + 1);
	}

	void _rotate_left(node_pointer oldRoot) noexcept {
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

		_AVLTreeCore::_update_height(oldRoot);
		_AVLTreeCore::_update_height(newRoot);
	}

	void _rotate_right(node_pointer oldRoot) noexcept {
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

		_AVLTreeCore::_update_height(oldRoot);
		_AVLTreeCore::_update_height(newRoot);
	}

	bool _try_rebalance(node_pointer node) noexcept {
		// Check for imbalance and rotate if needed
		const auto nodeBalance = _AVLTreeCore::_get_balance_factor(node);
		if (nodeBalance < -1) { // Subtree at node is imbalance to the left
			const auto leftBalance = _AVLTreeCore::_get_balance_factor(node->left);
			if (leftBalance <= 0) { // Left - Left
				this->_rotate_right(node);
				return true;
			}
			else { // Left - Right
				this->_rotate_left(node->left);
				this->_rotate_right(node);
				return true;
			}
		}

		if (nodeBalance > 1) { // Subtree at node is imbalance to the right
			const auto rightBalance = _AVLTreeCore::_get_balance_factor(node->right);
			if (rightBalance >= 0) { // Right - Right
				this->_rotate_left(node);
				return true;
			}
			else { // Right - Left
				this->_rotate_right(node->right);
				this->_rotate_left(node);
				return true;
			}
		}
		return false;
	}

	void _fix_tree(node_pointer node, node_pointer newNode) noexcept {
		// Travel upwards from node to root, update node height and rebalance if needed
		_AVLTreeCore::_update_height(newNode); // Reset node height for correct rebalancing

		for (;;) {
			if (node->isNil) { // Reach head before rebalancing
				return;
			}

			_AVLTreeCore::_update_height(node);
			if (this->_try_rebalance(node)) { // Rebalance, stop trying
				break;
			}
			node = node->parent;
		}

		for (;;) { // Update the remaining nodes height
			node = node->parent;
			if (node->isNil) {
				return;
			}

			_AVLTreeCore::_update_height(node);
		}
	}
};

template<class T, class Comp = std::less<T>>
class AVLTree : public _BSTree<
	_BSTreeTraits<T, T, Comp, _AVLTreeNode, false>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _AVLTreeNode, false>, _AVLTreeCore>;

public:
	using _BaseTree::_BaseTree;

	void swap(AVLTree& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(AVLTree<T, Comp>& lhs, AVLTree<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const AVLTree<T, Comp>& lhs, const AVLTree<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(
	const AVLTree<T, Comp>& lhs, const AVLTree<T, Comp>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}

template<class T, class Comp = std::less<T>>
class AVLMultiTree : public _BSTree<
	_BSTreeTraits<T, T, Comp, _AVLTreeNode, true>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _AVLTreeNode, true>, _AVLTreeCore>;

public:
	using _BaseTree::_BaseTree;

	void swap(AVLMultiTree& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(AVLMultiTree<T, Comp>& lhs, AVLMultiTree<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const AVLMultiTree<T, Comp>& lhs, const AVLMultiTree<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(
	const AVLMultiTree<T, Comp>& lhs, const AVLMultiTree<T, Comp>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}
#endif // AVL_TREE_H