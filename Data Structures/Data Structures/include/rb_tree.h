#pragma once
#ifndef RB_TREE_H
#define RB_TREE_H

#include"tree_core.h"

enum _NodeColor : bool {
	RED,
	BLACK
};

template<class ValueT>
struct _RBTreeNode : public _BSTreeNode<ValueT, _RBTreeNode<ValueT>> {
private:
	using _BaseNode = _BSTreeNode<ValueT, _RBTreeNode<ValueT>>;

public:
	using node_pointer	= typename _BaseNode::node_pointer;
	using value_type	= typename _BaseNode::value_type;

	_RBTreeNode() = default;

	_RBTreeNode(const _RBTreeNode&)				= delete;
	_RBTreeNode& operator=(const _RBTreeNode&)	= delete;

	[[nodiscard]] static node_pointer construct_head() {
		// Construct empty head sentinel with no value
		const auto newHead = _BaseNode::construct_head();
		newHead->color = BLACK;
		return newHead;
	}

	template<class... Args>
	[[nodiscard]] static node_pointer construct_node(node_pointer head, Args&&... args) {
		// Construct node with value from args
		const auto newNode = _BaseNode::construct_node(head, std::forward<Args>(args)...);
		newNode->color = RED;
		return newNode;
	}

	_NodeColor color; // 1 byte boolean, whether node is red or black
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class _RBTreeCore : public _BSTreeCore<ValueT, SizeT, DiffT, Ptr, ConstPtr, NodeT> {
private:
	using _BaseVal = _BSTreeCore<ValueT, SizeT, DiffT, Ptr, ConstPtr, NodeT>;

public:
	using node_type		= typename _BaseVal::node_type;
	using node_pointer	= typename _BaseVal::node_pointer;

public:
	using _BaseVal::head;
	using _BaseVal::size;

	_RBTreeCore() = default;

	node_pointer insert(const _NodeLocation<node_pointer> location, node_pointer newNode) noexcept {
		// Insert newNode at location
		newNode->color	= RED; // Inserted node must be red
		newNode->parent = location.parent;
		if (location.parent == head) {
			head->left		= newNode;
			head->right		= newNode;
			head->parent	= newNode;
			newNode->color	= BLACK; // Root node must be black
			return newNode;
		}

		if (location.child == _NodeChild::LEFT) {
			location.parent->left = newNode;
			if (location.parent == head->left) {
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
		this->_fix_insert(newNode);
		return newNode;
	}

	node_pointer extract(_BSTreeConstIterator<_RBTreeCore> where) noexcept {
		// Extract node pointer by where
		const node_pointer extracted = where.ptr;
		++where; // Advance iterator for return

		node_pointer fixNode;
		node_pointer fixParent;
		node_pointer currNode = extracted;

		if (currNode->left->isNil) {
			fixNode = currNode->right;
		}
		else if (currNode->right->isNil) {
			fixNode = currNode->left;
		}
		else {
			currNode	= where.ptr;
			fixNode		= currNode->right;
		}

		if (currNode == extracted) { // At most one subtree to reattach
			fixParent = extracted->parent;
			if (!fixNode->isNil) {
				fixNode->parent = fixParent;
			}

			if (head->parent == extracted) {
				head->parent = fixNode;
			}
			else if (fixParent->left == extracted) {
				fixParent->left = fixNode;
			}
			else {
				fixParent->right = fixNode;
			}

			if (head->left == extracted) {
				head->left = fixNode->isNil ? fixParent : _BaseVal::min(fixNode);
			}

			if (head->right == extracted) {
				head->right = fixNode->isNil ? fixParent : _BaseVal::max(fixNode);
			}
		}
		else { // Two subtrees to reattach
			extracted->left->parent = currNode;
			currNode->left = extracted->left;

			if (currNode == extracted->right) {
				fixParent = currNode;
			}
			else {
				fixParent = currNode->parent;
				if (!fixNode->isNil) {
					fixNode->parent = fixParent;
				}

				fixParent->left				= fixNode;
				currNode->right				= extracted->right;
				extracted->right->parent	= currNode;
			}

			if (head->parent == extracted) {
				head->parent = currNode;
			}
			else if (extracted->parent->left == extracted) {
				extracted->parent->left = currNode;
			}
			else {
				extracted->parent->right = currNode;
			}

			currNode->parent = extracted->parent;
			std::swap(currNode->color, extracted->color);
		}

		if (size > 0) {
			--size;
		}

		if (extracted->color == BLACK) {
			this->_fix_extract(fixNode, fixParent);
		}
		return extracted;
	}

	template<class ValueT>
	node_pointer copy_node(node_pointer node, ValueT&& val) {
		// Construct new node by copying or moving from node->value, preserving metadata
		const auto newNode = node_type::construct_node(head, std::forward<ValueT>(val));
		newNode->color = node->color;
		return newNode;
	}

private:
	void _fix_insert(node_pointer newNode) {
		for (node_pointer currNode = newNode; currNode->parent->color == RED;) {
			if (currNode->parent == currNode->parent->parent->left) { // Red-red in left subtree
				const node_pointer parent_sibling = currNode->parent->parent->right;
				if (parent_sibling->color == RED) { // Parent's currNode is red
					parent_sibling->color			= BLACK;
					currNode->parent->color			= BLACK;
					currNode->parent->parent->color = RED;
					currNode = currNode->parent->parent;
				}
				else { // Parent's currNode is black
					if (currNode == currNode->parent->right) { // currNode is right child
						currNode = currNode->parent;
						this->rotate_left(currNode);
					}

					currNode->parent->color			= BLACK;
					currNode->parent->parent->color = RED;
					this->rotate_right(currNode->parent->parent);
				}
			}
			else { // Red-red in right subtree
				const node_pointer parent_sibling = currNode->parent->parent->left;
				if (parent_sibling->color == RED) { // Parent's currNode is red
					parent_sibling->color			= BLACK;
					currNode->parent->color			= BLACK;
					currNode->parent->parent->color = RED;
					currNode = currNode->parent->parent;
				}
				else { // Parent's currNode is black
					if (currNode == currNode->parent->left) { // currNode is left child
						currNode = currNode->parent;
						this->rotate_right(currNode);
					}

					currNode->parent->color			= BLACK;
					currNode->parent->parent->color = RED;
					this->rotate_left(currNode->parent->parent);
				}
			}
		}

		head->parent->color = BLACK; // Root node must be black
	}

	void _fix_extract(node_pointer fixNode, node_pointer fixParent) {
		for (; fixNode != head->parent && fixNode->color == BLACK; fixParent = fixNode->parent) {
			if (fixNode == fixParent->left) { // Fix left subtree
				node_pointer currNode = fixParent->right;
				if (currNode->color == RED) {
					currNode->color		= BLACK;
					fixParent->color	= RED;
					this->rotate_left(fixParent);
					currNode = fixParent->right;
				}

				if (currNode->isNil) {
					fixNode = fixParent;
				}
				else if (currNode->left->color == BLACK && currNode->right->color == BLACK) {
					currNode->color = RED;
					fixNode = fixParent;
				}
				else {
					if (currNode->right->color == BLACK) {
						currNode->color			= RED;
						currNode->left->color	= BLACK;
						this->rotate_right(currNode);
						currNode = fixParent->right;
					}

					currNode->color			= fixParent->color;
					currNode->right->color	= BLACK;
					fixParent->color		= BLACK;
					this->rotate_left(fixParent);
					break;
				}
			}
			else { // Fix right subtree
				node_pointer currNode = fixParent->left;
				if (currNode->color == RED) {
					currNode->color		= BLACK;
					fixParent->color	= RED;
					this->rotate_right(fixParent);
					currNode = fixParent->left;
				}

				if (currNode->isNil) {
					fixNode = fixParent;
				}
				else if (currNode->left->color == BLACK && currNode->right->color == BLACK) {
					currNode->color = RED;
					fixNode = fixParent;
				}
				else {
					if (currNode->left->color == BLACK) {
						currNode->color			= RED;
						currNode->right->color	= BLACK;
						this->rotate_left(currNode);
						currNode = fixParent->left;
					}
					currNode->color			= fixParent->color;
					currNode->left->color	= BLACK;
					fixParent->color		= BLACK;
					this->rotate_right(fixParent);
					break;
				}
			}
		}

		fixNode->color = BLACK;
	}
};

template<class T, class Comp = std::less<T>>
class RBTree : public _BSTree<
	_BSTreeTraits<T, T, Comp, _RBTreeNode, false>,
	_RBTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _RBTreeNode, false>, _RBTreeCore>;

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

	void swap(RBTree& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(RBTree<T, Comp>& lhs, RBTree<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const RBTree<T, Comp>& lhs, const RBTree<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(
	const RBTree<T, Comp>& lhs, const RBTree<T, Comp>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}

template<class T, class Comp = std::less<T>>
class RBMultiTree : public _BSTree<
	_BSTreeTraits<T, T, Comp, _RBTreeNode, true>,
	_RBTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _RBTreeNode, true>, _RBTreeCore>;

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

	void swap(RBMultiTree& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(RBMultiTree<T, Comp>& lhs, RBMultiTree<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const RBMultiTree<T, Comp>& lhs, const RBMultiTree<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(
	const RBMultiTree<T, Comp>& lhs, const RBMultiTree<T, Comp>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // RB_TREE_H