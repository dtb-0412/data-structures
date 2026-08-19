#pragma once
#ifndef TREE_COMMON_H
#define TREE_COMMON_H

#include"node_handle.h"

template<
	class KeyT,
	class T,
	class Comp,
	bool _isMulti
>
class _TreeTraits {
public:
	using key_type		= KeyT;
	using value_type	= T;
	using key_compare	= Comp;
	using value_compare = key_compare;

	using node_handle	= _NodeHandle<
		_AVLTreeNode<value_type, uint8_t, int8_t>, _NodeHandleBase, key_type
	>;

	static constexpr bool isMulti	= _isMulti;
	static constexpr bool isMap		= false;

	static const key_type& key_from_node(const value_type& val) {
		return val;
	}
};

template<
	class KeyT,
	class T,
	class Comp,
	bool _isMulti
>
class _TreeMapTraits {
public:
	using key_type		= KeyT;
	using value_type	= std::pair<const key_type, T>;
	using key_compare	= Comp;

	using node_handle	= _NodeHandle<
		_AVLTreeNode<value_type, uint8_t, int8_t>, _NodeHandleMapBase, key_type, T
	>;

	static constexpr bool isMulti	= _isMulti;
	static constexpr bool isMap		= true;

	class value_compare {
		[[nodiscard]] bool operator()(const value_type& lhs, const value_type& rhs) const {
			// Compare nodes by comparing map key
			return comp(lhs.first, rhs.first);
		}

	protected:
		friend _AVLTree<_TreeMapTraits>;

		value_compare(key_compare comp) // Prevent creation from user
			: comp(comp) {}

		key_compare comp;
	};

	template<class T1, class T2>
	static const key_type& key_from_node(const std::pair<T1, T2>& val) {
		return val.first;
	}
};
#endif // TREE_COMMON_H