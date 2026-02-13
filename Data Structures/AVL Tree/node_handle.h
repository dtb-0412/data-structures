#pragma once
#ifndef NODE_HANDLE_H
#define NODE_HANDLE_H

#include"avl_tree.h"

#include"memory.hpp"
#include"utility.hpp"

template<class Iter, class NodeType>
struct InsertReturnType {
	Iter position; // Inserted node iterator if inserted, otherwise the duplicate node iterator
	bool inserted; // Whether insertion took place
	NodeType node; // Node handle: Empty if inserted, otherwise contains the node that was not inserted
};

template<class _DerivedType, class _ValueType>
struct _NodeHandleSetBase {
	using ValueType = _ValueType;

	ValueType& value() const noexcept {
		const auto& self = static_cast<const _DerivedType&>(*this);
		return self.getPointer()->value;
	}
};

template<class _NodeType, template<class...> class _Base, class... _Types> // CRTP
class _NodeHandle : public _Base<_NodeHandle<_NodeType, _Base, _Types...>, _Types...> {
	// Storage for a node from one of the node-based standard containers
private:
	using NodePointer = _NodeType*;
	
	_NodeHandle(const NodePointer ptr) noexcept
		: _ptr(ptr) {}

public:
	template<class, class>
	friend class AVLTree;

	_NodeHandle() noexcept
		: _ptr(nullptr) {}

	_NodeHandle(const _NodeHandle&)				= delete;
	_NodeHandle& operator=(const _NodeHandle&)	= delete;

	_NodeHandle(_NodeHandle&& other) noexcept
		: _ptr(std::exchange(other._ptr, nullptr)) {}

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

	[[nodiscard]] bool isEmpty() const noexcept {
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
			_NodeType::freeNode(std::exchange(_ptr, nullptr));
		}
	}

	NodePointer _release() noexcept {
		return std::exchange(_ptr, nullptr);
	}

private:
	NodePointer _ptr;
};
#endif // NODE_HANDLE_H