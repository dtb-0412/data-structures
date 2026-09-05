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

	height_type	height;	// 1 byte unsigned int, assuming BinarySearch tree height <= 255
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
		newNode->parent = location.parent;
		if (location.parent == head) { // First node in tree
			head->left		= newNode;
			head->right		= newNode;
			head->parent	= newNode;
			return newNode;
		}

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

		++size;
		this->_fix_tree(location.parent, newNode);
		return newNode;
	}

	node_pointer extract(_BSTreeConstIterator<_AVLTreeCore> where) noexcept {
		// Extract node pointed by where
		const node_pointer extracted = where.ptr; // UB: where == _BSTree::end()

		if (size == 0) { // Extract final node
			head->left		= head;
			head->right		= head;
			head->parent	= head;
			return extracted;
		}
		
		if (extracted == head->left) { // Extract leftmost node
			head->left = (++_BSTreeConstIterator(where)).ptr;
		}
		if (extracted == head->right) { // Extract rightmost node
			head->right = (--_BSTreeConstIterator(where)).ptr;
		}

		node_pointer fixNode	= head;
		node_pointer parent		= extracted->parent;
		if (!(extracted->left->isNil || extracted->right->isNil)) { // Node has both children
			const node_pointer successor = this->min(extracted->right);
			fixNode = (successor->parent != extracted) ? successor->parent : successor;

			if (successor->parent != extracted) {
				successor->parent->left = successor->right;
				if (!successor->right->isNil) {
					successor->right->parent = successor->parent;
				}
				successor->right = extracted->right;
				extracted->right->parent = successor;
			}

			extracted->parent->replace_child(extracted, successor);

			successor->left = extracted->left;
			extracted->left->parent = successor;

			successor->height = extracted->height;
		}
		else {
			const node_pointer childNode = !extracted->left->isNil
				? extracted->left
				: extracted->right;

			fixNode = extracted->parent;
			extracted->parent->replace_child(extracted, childNode);

			if (!childNode->isNil) {
				childNode->parent = extracted->parent;
			}
		}

		if (size > 0) {
			--size;
		}

		this->_fix_tree(fixNode, extracted);
		return extracted;
	}

	template<class ValueT>
	node_pointer copy_node(node_pointer node, ValueT&& val) {
		// Construct new node by copying or moving from node->value, preserving metadata
		const auto newNode = node_type::construct_node(head, std::forward<ValueT>(val));
		newNode->height = node->height;
		return newNode;
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
		_BaseVal::rotate_left(oldRoot);

		_AVLTreeCore::_update_height(oldRoot);
		_AVLTreeCore::_update_height(oldRoot->parent);
	}

	void _rotate_right(node_pointer oldRoot) noexcept {
		// Perform clockwise rotation on subtree at oldRoot
		_BaseVal::rotate_right(oldRoot);

		_AVLTreeCore::_update_height(oldRoot);
		_AVLTreeCore::_update_height(oldRoot->parent);
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
	using key_type		= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= typename _BaseTree::value_type;
	using size_type			= typename _BaseTree::size_type;
	using difference_type	= typename _BaseTree::difference_type;
	using pointer			= typename _BaseTree::pointer;
	using const_pointer		= typename _BaseTree::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseTree::iterator;
	using const_iterator	= typename _BaseTree::const_iterator;

	using reverse_iterator			= typename _BaseTree::reverse_iterator;
	using const_reverse_iterator	= typename _BaseTree::const_reverse_iterator;

	using node_handle			= typename _BaseTree::node_handle;
	using insert_return_type	= typename _BaseTree::insert_return_type;

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
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
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
	using key_type		= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= typename _BaseTree::value_type;
	using size_type			= typename _BaseTree::size_type;
	using difference_type	= typename _BaseTree::difference_type;
	using pointer			= typename _BaseTree::pointer;
	using const_pointer		= typename _BaseTree::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseTree::iterator;
	using const_iterator	= typename _BaseTree::const_iterator;

	using reverse_iterator			= typename _BaseTree::reverse_iterator;
	using const_reverse_iterator	= typename _BaseTree::const_reverse_iterator;

	using node_handle			= typename _BaseTree::node_handle;
	using insert_return_type	= typename _BaseTree::insert_return_type;

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
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // AVL_TREE_H