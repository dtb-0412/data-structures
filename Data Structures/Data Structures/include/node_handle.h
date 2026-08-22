#pragma once
#ifndef NODE_HANDLE_H
#define NODE_HANDLE_H

#include"avl_tree.h"

template<class It, class NodeHndl>
struct _InsertReturnType {
	It			position;	// Inserted node iterator if inserted, otherwise the duplicate node iterator
	bool		inserted;	// Whether insertion took place
	NodeHndl	node;		// Node handle: Empty if inserted, otherwise contains the node that was not inserted
};

template<class DerivedT, class T>
struct _NodeHandleBase {
	using value_type = T;

	value_type& value() const noexcept {
		const auto& self = static_cast<const DerivedT&>(*this);
		return self.get_pointer()->value;
	}
};

template<class DerivedT, class KeyT, class T>
struct _NodeHandleMapBase {
	using key_type		= KeyT;
	using mapped_type	= T;

	key_type& key() const noexcept {
		return this->_get_mutable_pair().first;
	}

	mapped_type& mapped() const noexcept {
		return this->_get_mutable_pair().second;
	}

private:
	/*
	C++17 standards require that when used as internal structure for map, tree node stores a key-value pair,
	with key being immutable. This is to prevent users from modifying node's key, compromising the tree's order.
	However, the standards also require node's key to be mutable after it is extracted from tree, when users can
	safely modify it.

	To enable this, we use reinterpret_cast to forcefully cast pair<const key, mapped>& to pair<key, mapped>&.
	
	Removing constness using const_cast or reinterpret_cast to overwrite an object that is initially const is
	considered UB by the standards.
	On MSVC compiler, reinterpret_cast between 2 layouts with the same size/alignment is practically safe.
	*/
	using _MutablePair = std::pair<key_type, mapped_type>;
	_MutablePair& _get_mutable_pair() const {
		const auto& self = static_cast<const DerivedT&>(*this);
		auto& data = self.get_pointer()->value;
		return reinterpret_cast<_MutablePair&>(data);
	}
};

/*
Node handle class serves as storage for nodes extracted from node-based containers.
CRTP is used to support static polymorphism instead of virtual functions because:
	- CRTP resolves polymorphism at compile time.
	- Virtual functions violates zero-overhead principle, adding extra footprint to every instance.
	- Virtual functions requires virtual destructor, introducing more runtime dispatch overhead.
*/
template<template<class...> class Base, class NodeT, class... Types>
class _NodeHandle : public Base<_NodeHandle<Base, NodeT, Types...>, Types...> {
private:
	using _NodePointer = typename NodeT::node_pointer;
	
	_NodeHandle(_NodePointer ptr) noexcept
		: _ptr(ptr) {}

public:
	template<class>
	friend class _AVLTree;

	_NodeHandle() noexcept
		: _ptr() {}

	_NodeHandle(const _NodeHandle&)				= delete;
	_NodeHandle& operator=(const _NodeHandle&)	= delete;

	_NodeHandle(_NodeHandle&& other) noexcept
		: _ptr(other._release()) {}

	_NodeHandle& operator=(_NodeHandle&& other) noexcept {
		// Always clear node handle, even when self-moving
		this->_clear();
		if (other._ptr && this != std::addressof(other)) { // Take ownership
			_ptr = other._release();
		}
		return *this;
	}

	~_NodeHandle() noexcept {
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

	void swap(_NodeHandle& other) noexcept {
		using std::swap;
		swap(_ptr, other._ptr);
	}

	friend void swap(_NodeHandle& lhs, _NodeHandle& rhs) noexcept {
		lhs.swap(rhs);
	}

	static _NodeHandle make(_NodePointer ptr) {
		return _NodeHandle(ptr);
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