#pragma once
#ifndef NODE_HANDLE_H
#define NODE_HANDLE_H

#include"avl_tree.h"

#include"memory.hpp"
#include"utility.hpp"

template<class Iter, class NodeT>
struct InsertReturnType {
	Iter position;	// Inserted node iterator if inserted, otherwise the duplicate node iterator
	bool inserted;	// Whether insertion took place
	NodeT node;		// Node handle: Empty if inserted, otherwise contains the node that was not inserted
};

template<class DerivedT, class ValueT>
struct NodeHandleSetBase {
	using ValueType = ValueT;

	ValueType& value() const noexcept {
		const auto& self = static_cast<const DerivedT&>(*this);
		return self.getPointer()->value;
	}
};

template<class NodeT, template<class...> class Base, class... Types> // CRTP
class NodeHandle : public Base<NodeHandle<NodeT, Base, Types...>, Types...> {
	// Storage for a node from one of the node-based standard containers
private:
	using NodePointer = NodeT*;
	
	NodeHandle(const NodePointer ptr) noexcept
		: _ptr(ptr) {}

public:
	template<class, class>
	friend class AVLTree;

	NodeHandle() noexcept
		: _ptr(nullptr) {}

	NodeHandle(const NodeHandle&)				= delete;
	NodeHandle& operator=(const NodeHandle&)	= delete;

	NodeHandle(NodeHandle&& other) noexcept
		: _ptr(std::exchange(other._ptr, nullptr)) {}

	NodeHandle& operator=(NodeHandle&& other) noexcept {
		// Always clear node handle, even when self-moving
		this->_clear();
		if (other._ptr && this != std::addressof(other)) { // Take ownership
			_ptr = std::exchange(other._ptr, nullptr);
		}
		return *this;
	}

	~NodeHandle() noexcept {
		this->_clear();
	}

	explicit operator bool() const noexcept {
		return _ptr != nullptr;
	}

	NodePointer getPointer() const noexcept {
		return _ptr;
	}

	[[nodiscard]] bool isEmpty() const noexcept {
		return _ptr == nullptr;
	}

	void swap(NodeHandle& Other) noexcept {
		using std::swap;
		swap(_ptr, Other._ptr); // ADL
	}

	friend void swap(NodeHandle& Left, NodeHandle& Right) noexcept {
		Left.swap(Right);
	}

	static NodeHandle make(NodePointer ptr) {
		ASSERT(ptr != nullptr, "Cannot make empty node handle");
		return NodeHandle(ptr);
	}

private:
	void _clear() noexcept {
		if (_ptr) {
			NodeT::freeNode(std::exchange(_ptr, nullptr));
		}
	}

	NodePointer _release() noexcept {
		return std::exchange(_ptr, nullptr);
	}

private:
	NodePointer _ptr;
};
#endif // NODE_HANDLE_H