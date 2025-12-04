#pragma once
#ifndef NODE_HANDLE_H
#define NODE_HANDLE_H

#include"memory.hpp"
#include"utility.hpp"

#include<iostream>
using std::cout;

template<class Iter, class NodeType>
struct InsertReturnType {
	Iter position; // Inserted node iterator
	bool inserted; // Whether insertion took place
	NodeType node; // Node handle: Empty if inserted, otherwise contains the node that was not inserted
};

template<class DerivedType, class ValueType>
struct _NodeHandleSetBase {
	using value_type = ValueType;

	value_type& value() const noexcept {
		const auto& self = static_cast<const DerivedType&>(*this);
		return self.getPointer()->value;
	}
};

template<class NodeType, template<class...> class Base, class... Types> // CRTP
class _NodeHandle : public Base<_NodeHandle<NodeType, Base, Types...>, Types...> {
	// Storage for a node from one of the node-based standard containers
private:
	using NodePointer = NodeType*;
	
	_NodeHandle(const NodePointer ptr) noexcept
		: _ptr(ptr) {}

public:
	_NodeHandle() noexcept {}

	_NodeHandle(const _NodeHandle&)				= delete;
	_NodeHandle& operator=(const _NodeHandle&)	= delete;

	_NodeHandle(_NodeHandle&& other) noexcept
		: _ptr(std::exchange(_ptr, other._ptr)) {}

	_NodeHandle& operator=(_NodeHandle&& other) noexcept {
		// Always clear node handle, even when self-moving
		this->_clear();
		if (other._ptr && this != std::addressof(other)) { // Take ownership
			_ptr = std::exchange(other._ptr, nullptr);
		}
		return *this;
	}

	~_NodeHandle() noexcept {
		this->_clear();
	}

	explicit operator bool() const noexcept {
		return _ptr != nullptr;
	}

	NodePointer getPointer() const noexcept {
		return _ptr;
	}

	[[nodiscard]] bool empty() const noexcept {
		return _ptr == nullptr;
	}

	void swap(_NodeHandle& Other) noexcept {
		using std::swap;
		swap(_ptr, Other._ptr); // ADL
	}

	friend void swap(_NodeHandle& Left, _NodeHandle& Right) noexcept {
		Left.swap(Right);
	}

	static _NodeHandle make(NodePointer ptr) {
		ASSERT(ptr != nullptr, "Cannot make empty node handle");
		return _NodeHandle(ptr);
	}

private:
	void _clear() noexcept {
		if (_ptr) {
			NodeType::freeNode(std::exchange(_ptr, nullptr));
		}
	}

private:
	NodePointer _ptr;
};
#endif // NODE_HANDLE_H