
inline int
intrin_index_of_first_set_bit(u32 value) {
    unsigned long result = 0;
    // TODO(Alexander): MSVC intrinsics, make compiler agnostic
    if (_BitScanForward(&result, value)) {
        return result;
    }
    return -1;
}

inline int
intrin_index_of_first_set_bit(u64 value) {
    unsigned long result = 0;
    // TODO(Alexander): MSVC intrinsics, make compiler agnostic
    if (_BitScanForward64(&result, value)) {
        return result;
    }
    return -1;
}

inline int
intrin_index_of_last_set_bit(u32 value) {
    unsigned long result = 0;
    // TODO(Alexander): MSVC intrinsics, make compiler agnostic
    if (_BitScanReverse(&result, value)) {
        return result;
    }
    return -1;
}

inline int
intrin_index_of_last_set_bit(u64 value) {
    unsigned long result = 0;
    // TODO(Alexander): MSVC intrinsics, make compiler agnostic
    if (_BitScanReverse64(&result, value)) {
        return result;
    }
    return -1;
}
