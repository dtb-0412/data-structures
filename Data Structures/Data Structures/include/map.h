#pragma once
#ifndef MAP_H
#define MAP_H

#include"avl_tree.h"

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

	using node_handle = _NodeHandle<
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
			: comp(comp) {
		}

		key_compare comp;
	};

	template<class T1, class T2>
	static const key_type& key_from_node(const std::pair<T1, T2>& val) {
		return val.first;
	}
};

template<class Key, class T, class Comp = std::less<>>
class Map : public _AVLTree<_TreeMapTraits<Key, T, Comp, false>> {

};

template<class Key, class T, class Comp = std::less<>>
class MultiMap : public _AVLTree<_TreeMapTraits<Key, T, Comp, true>> {

};
#endif // MAP_H