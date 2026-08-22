#pragma once
#ifndef MAP_H
#define MAP_H

#include"avl_tree.h"

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

template<class KeyT, class... Args>
struct _InPlaceMapKeyExtractorBase {
	static constexpr bool isExtractable = false;
};

template<class KeyT, class T>
struct _InPlaceMapKeyExtractorBase<KeyT, KeyT, T> {
	// If we would call the emplace family with (key, value), we can use the first parameter as the key
	static constexpr bool isExtractable = true;

	static const KeyT& extract(const KeyT& key, const T&) noexcept {
		return key;
	}
};

template<class KeyT, class First, class Second>
struct _InPlaceMapKeyExtractorBase<KeyT, std::pair<First, Second>> {
	// If we would call the emplace family with std::pair<first, second>, we can use the first parameter as the key
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<First>>;

	static const KeyT& extract(const std::pair<First, Second>& val) noexcept {
		return val.first;
	}
};

template<class KeyT, class First, class... Args>
struct _InPlaceMapKeyExtractorBase<KeyT, std::piecewise_construct_t, std::tuple<First>, std::tuple<Args...>> {
	/*
	If we would call the emplace family with std::piecewise_construct_t, we can use std::get<0>() on the first tuple as the key
	This is a very niche case, used only when:
		- Mapped type is non-copyable and non-movable (we cannot create temporary object, then copy/move into tuple)
		- Mapped type must be constructed in place at node's memory (std::forward_as_tuple() must be used here)
	*/
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<First>>;

	static const KeyT& extract(
		const std::piecewise_construct_t&, const std::tuple<First>& key, const std::tuple<Args...>&
	) noexcept {
		return std::get<0>(key);
	}
};

/*
From C++23, std::pair can be constructed from tuple-like objects, specifically std::tuple<U, V> and std::array<T, 2>.
If we would call the emplace family with std::tuple<U, V> or std::array<T, 2>, we can use std::get<0> or operator[](0)
to get the first element as key.
*/
#if _MSVC_LANG > 202002L
template<class KeyT, class T>
struct _InPlaceMapKeyExtractorBase<KeyT, std::array<T, 2>> {
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<T>>;

	static const KeyT& extract(const std::array<T, 2>& val) noexcept {
		return val[0];
	}
};

template<class KeyT, class First, class Second>
struct _InPlaceMapKeyExtractorBase<KeyT, std::tuple<First, Second>> {
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<First>>;

	static const KeyT& extract(const std::tuple<First, Second>& val) noexcept {
		return std::get<0>(val);
	}
};
#endif // From C++23

template<class... Args>
using _InPlaceMapKeyExtractor = _InPlaceMapKeyExtractorBase<std::remove_cvref_t<Args>...>;

template<class KeyT, class T, class Comp, template<class> class NodeT, bool _isMulti>
class _BSTreeMapTraits {
public:
	using key_type		= KeyT;
	using value_type	= std::pair<const key_type, T>;
	using key_compare	= Comp;

	using node_type		= NodeT<value_type>;
	using node_pointer	= typename node_type::node_pointer;

	using node_handle	= _NodeHandle<_NodeHandleMapBase, node_type, key_type, T>;

	class value_compare {
		[[nodiscard]] bool operator()(const value_type& lhs, const value_type& rhs) const {
			// Compare nodes by comparing map key
			return comp(lhs.first, rhs.first);
		}

	protected:
		template<class, template<class...> class>
		friend class _BSTree;

		value_compare(key_compare comp) // Prevent creation from user
			: comp(comp) {
		}

		key_compare comp;
	};

	static constexpr bool isMulti	= _isMulti;
	static constexpr bool isMap		= true;

	template<class... Args>
	using in_place_key_extractor = _InPlaceMapKeyExtractor<key_type, Args...>;

	template<class T1, class T2>
	static const key_type& key_from_node(const std::pair<T1, T2>& val) {
		return val.first;
	}
};

template<class KeyT, class T, class Comp = std::less<T>>
class Map : public _BSTree<
	_BSTreeMapTraits<KeyT, T, Comp, _AVLTreeNode, false>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeMapTraits<KeyT, T, Comp, _AVLTreeNode, false>, _AVLTreeCore>;

public:
	using _BaseTree::_BaseTree;

	// ...

	void swap(Map& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(Map<T, Comp>& lhs, Map<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const Map<T, Comp>& lhs, const Map<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const Map<T, Comp>& lhs, const Map<T, Comp>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}

template<class KeyT, class T, class Comp = std::less<T>>
class MultiMap : public _BSTree<
	_BSTreeMapTraits<KeyT, T, Comp, _AVLTreeNode, true>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree< _BSTreeMapTraits<KeyT, T, Comp, _AVLTreeNode, true>, _AVLTreeCore>;

public:
	using _BaseTree::_BaseTree;

	// ...

	void swap(MultiMap& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(MultiMap<T, Comp>& lhs, MultiMap<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const MultiMap<T, Comp>& lhs, const MultiMap<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const MultiMap<T, Comp>& lhs, const MultiMap<T, Comp>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}
#endif // MAP_H