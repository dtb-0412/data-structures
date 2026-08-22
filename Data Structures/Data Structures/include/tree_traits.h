#pragma once
#ifndef TREE_TRAITS_H
#define TREE_TRAITS_H

#include"node_handle.h"

template<class KeyT, class... Args>
struct _InPlaceKeyExtractorBase {
	// By default we can't extract the key in the emplace family and must construct a node we might not use
	static constexpr bool isExtractable = false;
};

template<class KeyT>
struct _InPlaceKeyExtractorBase<KeyT, KeyT> {
	static constexpr bool isExtractable = true;

	static const KeyT& extract(const KeyT& key) noexcept {
		return key;
	}
};

template<class... Args>
using _InPlaceKeyExtractor = _InPlaceKeyExtractorBase<std::remove_cvref_t<Args>...>;

template<class KeyT, class T, class Comp, template<class> class NodeT, bool _isMulti>
class _TreeTraits {
public:
	using key_type		= KeyT;
	using value_type	= T;
	using key_compare	= Comp;
	using value_compare = key_compare;

	using node_type		= NodeT<value_type>;
	using node_pointer	= typename node_type::node_pointer;

	using node_handle	= _NodeHandle<_NodeHandleBase, node_type, key_type>;

	static constexpr bool isMulti	= _isMulti;
	static constexpr bool isMap		= false;

	template<class... Args>
	using in_place_key_extractor = _InPlaceKeyExtractor<key_type, Args...>;

	static const key_type& key_from_node(const value_type& val) {
		return val;
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
class _TreeMapTraits {
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
		friend _AVLTree<_TreeMapTraits>;

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
#endif // TREE_TRAITS_H