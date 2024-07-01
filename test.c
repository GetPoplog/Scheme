
    typedef     int* List;
    typedef     int* Set;
    typedef     int Boolean;
    Set         empty_set;
    Set         list_to_set(List l);
    Set         intersect(Set s1, Set s2);
    Boolean     member_set(int, Set s);
    Boolean     included_in(Set s1, Set s2);
    Boolean     equal_set(Set s1, Set s2);
    Set         intersect(Set s1, Set s2);
