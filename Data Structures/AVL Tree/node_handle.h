#pragma once
#ifndef NODE_HANDLE_H
#define NODE_HANDLE_H

#include"memory.hpp"

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

	void _clear() noexcept {
		if (_ptr) {
			NodeType::freeNode(_ptr);
			_ptr = nullptr;
		}
	}

public:
	_NodeHandle() noexcept {}

	_NodeHandle(const _NodeHandle&)				= delete;
	_NodeHandle& operator=(const _NodeHandle&)	= delete;

	_NodeHandle(_NodeHandle&& other) noexcept
		: _ptr(std::exchange(_ptr, other._ptr)) {}

	_NodeHandle& operator=(_NodeHandle&& other) noexcept {
		if (this == std::addressof(other)) {
			return *this;
		}

		if (_ptr) {
			memory::destructInPlace(std::addressof(_ptr->value));
			memory::deallocate(_ptr, 1);
		}

		_ptr = std::exchange(other._ptr, nullptr);
		return *this;
	}

	~_NodeHandle() noexcept {
		_clear();
	}

	explicit operator bool() const noexcept {
		return _ptr != nullptr;
	}

public:
	NodePointer getPointer() const noexcept {
		return _ptr;
	}

	void swap(_NodeHandle& Other) noexcept {
		using std::swap;
		swap(_ptr, Other._ptr); // ADL
	}

	friend void swap(_NodeHandle& Left, _NodeHandle& Right) noexcept {
		Left.swap(Right);
	}

private:
	NodePointer _ptr;
};
#endif // NODE_HANDLE_H