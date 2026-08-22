#pragma once
#ifndef MAP_H
#define MAP_H

#include"avl_tree.h"

template<class KeyT, class T, class Comp = std::less<>>
class Map : public _AVLTree<_TreeMapTraits<KeyT, T, Comp, _AVLTreeNode, false>> {

};

template<class KeyT, class T, class Comp = std::less<>>
class MultiMap : public _AVLTree<_TreeMapTraits<KeyT, T, Comp, _AVLTreeNode, true>> {

};
#endif // MAP_H