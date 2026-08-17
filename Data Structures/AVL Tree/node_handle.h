#pragma once
#ifndef NODE_HANDLE_H
#define NODE_HANDLE_H

#include"avl_tree.h"

#include"memory.hpp"
#include"utility.hpp"

template<class It, class NodeT>
struct InsertReturnType {
	It		position;	// Inserted node iterator if inserted, otherwise the duplicate node iterator
	bool	inserted;	// Whether insertion took place
	NodeT	node;		// Node handle: Empty if inserted, otherwise contains the node that was not inserted
};

template<class DerivedT, class ValueT>
struct NodeHandleSetBase {
	using value_type = ValueT;

	value_type& value() const noexcept {
		const auto& self = static_cast<const DerivedT&>(*this);
		return self.get_pointer()->value;
	}
};

template<class NodeT, template<class...> class Base, class... Types> // CRTP
class NodeHandle : public Base<NodeHandle<NodeT, Base, Types...>, Types...> {
	// Storage for a node from one of the node-based standard containers
private:
	using _NodePointer = typename NodeT::node_pointer;
	
	NodeHandle(_NodePointer ptr) noexcept
		: _ptr(ptr) {}

public:
	template<class, class>
	friend class AVLTree;

	NodeHandle() noexcept
		: _ptr() {}

	NodeHandle(const NodeHandle&)				= delete;
	NodeHandle& operator=(const NodeHandle&)	= delete;

	NodeHandle(NodeHandle&& other) noexcept
		: _ptr(other._release()) {}

	NodeHandle& operator=(NodeHandle&& other) noexcept {
		// Always clear node handle, even when self-moving
		this->_clear();
		if (other._ptr && this != std::addressof(other)) { // Take ownership
			_ptr = other._release();
		}
		return *this;
	}

	~NodeHandle() noexcept {
		this->_clear();
	}

	explicit operator bool() const noexcept {
		return _ptr != nullptr;
	}

	_NodePointer get_pointer() const noexcept {
		return _ptr;
	}

	[[nodiscard]] bool is_empty() const noexcept {
		return _ptr == nullptr;
	}

	void swap(NodeHandle& other) noexcept {
		using std::swap;
		swap(_ptr, other._ptr);
	}

	friend void swap(NodeHandle& lhs, NodeHandle& rhs) noexcept {
		lhs.swap(rhs);
	}

	static NodeHandle make(_NodePointer ptr) {
		return NodeHandle(ptr);
	}

private:
	void _clear() noexcept {
		if (_ptr) {
			NodeT::free_node(this->_release());
		}
	}

	_NodePointer _release() noexcept {
		return std::exchange(_ptr, nullptr);
	}

private:
	_NodePointer _ptr;
};
#endif // NODE_HANDLE_H