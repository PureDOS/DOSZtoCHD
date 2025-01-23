/*
 *  DOSZtoCHD
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <memory.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <vector>
#include <string>
#ifdef _WIN32
#include <windows.h>
#endif

typedef unsigned char Bit8u;
typedef unsigned short Bit16u;
typedef signed short Bit16s;
typedef unsigned int Bit32u;
typedef signed int Bit32s;
#ifndef _MSC_VER
typedef unsigned long long Bit64u;
typedef signed long long Bit64s;
#else
typedef unsigned __int64 Bit64u;
typedef signed __int64 Bit64s;
#endif

// Use 64-bit fseek and ftell
#if defined(_MSC_VER) && _MSC_VER >= 1400 // VC2005 and up have a special 64-bit fseek
#define fseek_wrap(fp, offset, whence) _fseeki64(fp, (__int64)offset, whence)
#define ftell_wrap(fp) _ftelli64(fp)
#elif defined(HAVE_64BIT_OFFSETS) || (defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE - 0) >= 200112) || (defined(__POSIX_VISIBLE) && __POSIX_VISIBLE >= 200112) || (defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112) || __USE_LARGEFILE || (defined(_FILE_OFFSET_BITS) && _FILE_OFFSET_BITS == 64)
#define fseek_wrap(fp, offset, whence) fseeko(fp, (off_t)offset, whence)
#define ftell_wrap(fp) ftello(fp)
#else
#define fseek_wrap(fp, offset, whence) fseek(fp, (long)offset, whence)
#define ftell_wrap(fp) ftell(fp)
#endif

static void LogErr(const char* fmt, ...) { va_list ap; va_start(ap, fmt); vfprintf(stderr, fmt, ap); va_end(ap); fflush(stderr); }
static void Log(const char* fmt, ...) { va_list ap; va_start(ap, fmt); vfprintf(stdout, fmt, ap); va_end(ap); fflush(stdout); }

#define ZIP_MAX(a,b) (((a)>(b))?(a):(b))
#define ZIP_MIN(a,b) (((a)<(b))?(a):(b))
#define ZIP_READ_LE16(p) ((Bit16u)(((const Bit8u *)(p))[0]) | ((Bit16u)(((const Bit8u *)(p))[1]) << 8U))
#define ZIP_READ_LE32(p) ((Bit32u)(((const Bit8u *)(p))[0]) | ((Bit32u)(((const Bit8u *)(p))[1]) << 8U) | ((Bit32u)(((const Bit8u *)(p))[2]) << 16U) | ((Bit32u)(((const Bit8u *)(p))[3]) << 24U))
#define ZIP_READ_LE64(p) ((Bit64u)(((const Bit8u *)(p))[0]) | ((Bit64u)(((const Bit8u *)(p))[1]) << 8U) | ((Bit64u)(((const Bit8u *)(p))[2]) << 16U) | ((Bit64u)(((const Bit8u *)(p))[3]) << 24U) | ((Bit64u)(((const Bit8u *)(p))[4]) << 32U) | ((Bit64u)(((const Bit8u *)(p))[5]) << 40U) | ((Bit64u)(((const Bit8u *)(p))[6]) << 48U) | ((Bit64u)(((const Bit8u *)(p))[7]) << 56U))
#define ZIP_READ_BE32(p) ((Bit32u)((((const Bit8u *)(p))[0] << 24) | (((const Bit8u *)(p))[1] << 16) | (((const Bit8u *)(p))[2] << 8) | ((const Bit8u *)(p))[3]))
#define ZIP_READ_BE64(p) ((Bit64u)((((Bit64u)((const Bit8u *)(p))[0] << 56) | ((Bit64u)((const Bit8u *)(p))[1] << 48) | ((Bit64u)((const Bit8u *)(p))[2] << 40) | ((Bit64u)((const Bit8u *)(p))[3] << 32) | ((Bit64u)((const Bit8u *)(p))[4] << 24) | ((Bit64u)((const Bit8u *)(p))[5] << 16) | ((Bit64u)((const Bit8u *)(p))[6] << 8) | (Bit64u)((const Bit8u *)(p))[7])))
#define ZIP_WRITE_LE16(b,v) { (b)[0] = (Bit8u)((Bit16u)(v)); (b)[1] = (Bit8u)((Bit16u)(v) >> 8); }
#define ZIP_WRITE_BE16(b,v) { (b)[0] = (Bit8u)((Bit16u)(v) >> 8); (b)[1] = (Bit8u)((Bit16u)(v)); }
#define ZIP_WRITE_LB16(b,v) { ZIP_WRITE_LE16(b,v) ZIP_WRITE_BE16((b+2),v) }
#define ZIP_WRITE_LE32(b,v) { (b)[0] = (Bit8u)((Bit32u)(v)); (b)[1] = (Bit8u)(((Bit32u)(v) >> 8)); (b)[2] = (Bit8u)(((Bit32u)(v) >> 16)); (b)[3] = (Bit8u)((Bit32u)(v) >> 24); }
#define ZIP_WRITE_BE32(b,v) { (b)[0] = (Bit8u)((Bit32u)(v) >> 24); (b)[1] = (Bit8u)(((Bit32u)(v) >> 16)); (b)[2] = (Bit8u)(((Bit32u)(v) >> 8)); (b)[3] = (Bit8u)((Bit32u)(v)); }
#define ZIP_WRITE_BE64(b,v) { (b)[0] = (Bit8u)((Bit64u)(v) >> 56); (b)[1] = (Bit8u)((Bit64u)(v) >> 48); (b)[2] = (Bit8u)((Bit64u)(v) >> 40); (b)[3] = (Bit8u)((Bit64u)(v) << 32); (b)[4] = (Bit8u)((Bit64u)(v) >> 24); (b)[5] = (Bit8u)((Bit64u)(v) >> 16); (b)[6] = (Bit8u)((Bit64u)(v) >> 8); (b)[7] = (Bit8u)(Bit64u)(v); }
#define ZIP_WRITE_LB32(b,v) { ZIP_WRITE_LE32(b,v) ZIP_WRITE_BE32((b+4),v) }
#define ZIP_PACKDATE(year,mon,day) (Bit16u)((((year)-1980)&0x7f)<<9 | ((mon)&0x3f) << 5 | ((day)&0x1f))
#define ZIP_PACKTIME(hour,min,sec) (Bit16u)(((hour)&0x1f)<<11 | ((min)&0x3f) << 5 | (((sec)/2)&0x1f))

#ifdef NDEBUG
#define ZIP_ASSERT(cond)
#else
#define ZIP_ASSERT(cond) (void)((cond) ? ((int)0) : *(volatile int*)0 |= 0xbad|fprintf(stderr, "FAILED ASSERT (%s)\n", #cond))
#endif

static FILE* fopen_utf8(const char* path, const char* mode)
{
	#ifdef _WIN32
	bool needw = false;
	for (const char* p = path; *p; p++) { if ((Bit8u)*p > 0x7F) { needw = true; break; } }
	if (needw)
	{
		WCHAR wpath[MAX_PATH+5], wmode[20], *pwmode = wmode;
		MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, MAX_PATH);
		for (const char* p = mode, *pEnd = p + 19; *p && p != pEnd; p++) *(pwmode++) = *p;
		*pwmode = '\0';
		return _wfopen(wpath, wmode);
	}
	#endif
	return fopen(path, mode);
}

static bool stat_utf8(const char* path, Bit16u& date, Bit16u& time)
{
	time_t mtime;
	#ifdef _WIN32
	bool needw = false;
	for (const char* p = path; *p; p++) { if ((Bit8u)*p > 0x7F) { needw = true; break; } }
	if (needw)
	{
		WCHAR wpath[MAX_PATH+5];
		MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, MAX_PATH);
		struct _stat64i32 wres;
		if (_wstat64i32(wpath, &wres)) return false;
		mtime = wres.st_mtime;
	}
	else
	#endif
	{
		struct stat res;
		if (stat(path, &res)) return false;
		mtime = res.st_mtime;
	}
	struct tm *mtm = localtime(&mtime);
	date = ZIP_PACKDATE(mtm->tm_year+1900, mtm->tm_mon + 1, mtm->tm_mday);
	time = ZIP_PACKTIME(mtm->tm_hour, mtm->tm_min, mtm->tm_sec);
	return true;
}

static std::string& PathSetFileExt(std::string& path, const char* ext)
{
	size_t i = path.rfind('.');
	return (i == std::string::npos ? path.append(1, '.') : path.erase(i + 1, path.size() - i - i)).append(ext);
}

struct SFile
{
	std::string path; Bit64u size; Bit16u date, time;
	inline SFile() : date(0), time(0) { }
	virtual inline ~SFile() { }
	virtual bool Open() = 0;
	virtual bool IsOpen() = 0;
	virtual bool Close() = 0;
	virtual Bit64u Read(Bit8u* data, Bit64u len) = 0;
	virtual Bit64u Seek(Bit64s ofs, int origin = SEEK_SET) = 0;
};

struct SFileRaw : SFile
{
	FILE* f;
	inline SFileRaw(std::string& _path, bool isdir) : SFile(), f(NULL)
	{
		path.swap(_path);
		if (!stat_utf8(path.c_str(), date, time)) { size = 0; return; }
		if (isdir) { path += '/'; size = 0; return; }
		if (!Open()) { size = 0; return; }
		size = Seek(0, SEEK_END);
		Close();
	}
	virtual inline ~SFileRaw() { Close(); }
	virtual bool Open() { ZIP_ASSERT(!f); return ((f = fopen_utf8(path.c_str(), "rb")) != NULL); }
	virtual bool IsOpen() { return (f != NULL); }
	virtual bool Close() { return (f ? (fclose(f), (f = NULL), true) : false); }
	virtual Bit64u Read(Bit8u* data, Bit64u len) { return (Bit64u)fread(data, 1, (size_t)len, f); }
	virtual Bit64u Seek(Bit64s ofs, int origin = SEEK_SET) { fseek_wrap(f, ofs, origin); return (origin == SEEK_SET ? ofs : ftell_wrap(f)); }
};

struct SFileMemory : SFile
{
	Bit8u* buf; Bit64u pos;
	inline SFileMemory(Bit64u _size) : buf(_size ? (Bit8u*)malloc((size_t)_size) : (Bit8u*)NULL), pos((Bit64u)-1) { size = _size; }
	virtual inline ~SFileMemory() { free(buf); }
	virtual bool Open() { ZIP_ASSERT(pos == (Bit64u)-1 && size); pos = 0; return true; }
	virtual bool IsOpen() { return (pos != (Bit64u)-1); }
	virtual bool Close() { pos = (Bit64u)-1; return true; }
	virtual Bit64u Read(Bit8u* data, Bit64u ln) { Bit64u i = pos+ln, j = (i > size ? size : i), k = (j - pos); memcpy(data, buf+pos, (size_t)k); pos += k; return k; }
	virtual Bit64u Seek(Bit64s ofs, int origin = SEEK_SET) { switch (origin) { case SEEK_SET: default: pos = (Bit64u)ofs; break; case SEEK_CUR: pos += ofs; break; case SEEK_END: pos = size + ofs; break; } return (pos > size ? (pos = size) : pos); }
};

struct SFileZip : SFileMemory
{
	struct ZipReader
	{
		ZipReader(SFile& _archive) : archive(_archive), refs(0) { }
		~ZipReader() { archive.Close(); }
		SFile& archive; Bit32u refs;
		void AddRef() { refs++; }
		void DelRef() 
		{
			ZIP_ASSERT(refs); 
			if (!--refs)
				delete this;
		}
	};

	ZipReader& reader; Bit64u unpacked, data_ofs; Bit32u comp_size; Bit8u bit_flags, method; bool lhskip; void* decomp_state;
	SFileZip(ZipReader& _reader, const char* filename, Bit32u filename_len, Bit64u _data_ofs, Bit32u _comp_size, Bit64u _decomp_size, Bit16u _date, Bit16u _time, Bit8u _bit_flags, Bit8u _method)
		: SFileMemory(0), reader(_reader), unpacked(0), data_ofs(_data_ofs), comp_size(_comp_size), bit_flags(_bit_flags), method(_method), lhskip(false), decomp_state(NULL)
	{
		(path.assign(reader.archive.path) += '/').append(filename, (size_t)filename_len);
		size = _decomp_size;
		date = _date;
		time = _time;
		reader.AddRef();
	}
	virtual inline ~SFileZip()
	{
		free(decomp_state);
		reader.DelRef();
	}
	virtual inline bool Open() { ZIP_ASSERT(size); return (SFileMemory::Open()); }
	virtual Bit64u Read(Bit8u* data, Bit64u len)
	{
		Bit64u posAndLen = pos+len, readEnd = (posAndLen > size ? size : posAndLen), readLen = (readEnd - pos);
		if (!readLen) return 0;
		if (method == METHOD_STORED)
		{
			if (!lhskip && !SkipLocalHeader()) return 0;
			if (reader.archive.Seek(data_ofs + pos) != data_ofs + pos) { ZIP_ASSERT(false); return 0; }
			pos += readLen; return reader.archive.Read(data, readLen);
		}
		if (readEnd > unpacked && !Unpack(readEnd)) { ZIP_ASSERT(false); return 0; }
		memcpy(data, buf+pos, (size_t)readLen); pos += readLen; return readLen;
	}

	// Various ZIP archive enums. To completely avoid cross platform compiler alignment and platform endian issues, we don't use structs for any of this stuff
	enum
	{
		// ZIP archive identifiers and record sizes
		ZIP_END_OF_CENTRAL_DIR_HEADER_SIG = 0x06054b50, ZIP_CENTRAL_DIR_HEADER_SIG = 0x02014b50, ZIP_LOCAL_DIR_HEADER_SIG = 0x04034b50,
		ZIP_LOCAL_DIR_HEADER_SIZE = 30, ZIP_CENTRAL_DIR_HEADER_SIZE = 46, ZIP_END_OF_CENTRAL_DIR_HEADER_SIZE = 22,
		ZIP64_END_OF_CENTRAL_DIR_HEADER_SIG = 0x06064b50, ZIP64_END_OF_CENTRAL_DIR_HEADER_SIZE = 56,
		ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIG = 0x07064b50, ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIZE = 20,
		// End of central directory offsets
		ZIP_ECDH_NUM_THIS_DISK_OFS = 4, ZIP_ECDH_NUM_DISK_CDIR_OFS = 6, ZIP_ECDH_CDIR_NUM_ENTRIES_ON_DISK_OFS = 8,
		ZIP_ECDH_CDIR_TOTAL_ENTRIES_OFS = 10, ZIP_ECDH_CDIR_SIZE_OFS = 12, ZIP_ECDH_CDIR_OFS_OFS = 16, ZIP_ECDH_COMMENT_SIZE_OFS = 20,
		ZIP64_ECDL_ECDH_OFS_OFS = 8, ZIP64_ECDH_CDIR_TOTAL_ENTRIES_OFS = 32, ZIP64_ECDH_CDIR_SIZE_OFS = 40, ZIP64_ECDH_CDIR_OFS_OFS = 48,
		// Central directory header record offsets
		ZIP_CDH_BIT_FLAG_OFS = 8, ZIP_CDH_METHOD_OFS = 10, ZIP_CDH_FILE_TIME_OFS = 12, ZIP_CDH_FILE_DATE_OFS = 14, ZIP_CDH_CRC32_OFS = 16,
		ZIP_CDH_COMPRESSED_SIZE_OFS = 20, ZIP_CDH_DECOMPRESSED_SIZE_OFS = 24, ZIP_CDH_FILENAME_LEN_OFS = 28, ZIP_CDH_EXTRA_LEN_OFS = 30,
		ZIP_CDH_COMMENT_LEN_OFS = 32, ZIP_CDH_EXTERNAL_ATTR_OFS = 38, ZIP_CDH_LOCAL_HEADER_OFS = 42,
		// Local directory header offsets
		ZIP_LDH_FILENAME_LEN_OFS = 26, ZIP_LDH_EXTRA_LEN_OFS = 28,
	};

	bool SkipLocalHeader()
	{
		ZIP_ASSERT(!lhskip);
		SFile& fi = reader.archive;
		Bit8u local_header[ZIP_LOCAL_DIR_HEADER_SIZE];
		if (fi.Seek(data_ofs) != data_ofs || fi.Read(local_header, ZIP_LOCAL_DIR_HEADER_SIZE) != ZIP_LOCAL_DIR_HEADER_SIZE)
			return false;
		if (ZIP_READ_LE32(local_header) != ZIP_LOCAL_DIR_HEADER_SIG)
			return false;
		data_ofs += ZIP_LOCAL_DIR_HEADER_SIZE + ZIP_READ_LE16(local_header + ZIP_LDH_FILENAME_LEN_OFS) + ZIP_READ_LE16(local_header + ZIP_LDH_EXTRA_LEN_OFS);
		if ((data_ofs + comp_size) > fi.size)
			return false;
		return (lhskip = true);
	}

	bool Unpack(Bit64u unpack_until);

	enum { METHOD_STORED = 0, METHOD_SHRUNK = 1, METHOD_IMPLODED = 6, METHOD_DEFLATED = 8 };
	static bool MethodSupported(Bit32u method) { return (method == METHOD_DEFLATED || method == METHOD_STORED || method == METHOD_SHRUNK || method == METHOD_IMPLODED); }

	static bool IndexFiles(SFile& fi, std::vector<SFile*>& files)
	{
		// Basic sanity checks - reject files which are too small.
		if (fi.size < ZIP_END_OF_CENTRAL_DIR_HEADER_SIZE)
		{
			invalid_zip:
			LogErr("Invalid or unsupported ZIP file: %s\n", fi.path.c_str());
			if (fi.size >= ZIP_END_OF_CENTRAL_DIR_HEADER_SIZE) fi.Close();
			return false;
		}

		// Find the end of central directory record by scanning the file from the end towards the beginning.
		fi.Open();
		Bit8u buf[4096];
		Bit64u ecdh_ofs = (fi.size < sizeof(buf) ? 0 : fi.size - sizeof(buf));
		for (;; ecdh_ofs = ZIP_MAX(ecdh_ofs - (sizeof(buf) - 3), 0))
		{
			Bit32s i, n = (Bit32s)ZIP_MIN(sizeof(buf), fi.size - ecdh_ofs);
			if (fi.Seek(ecdh_ofs) != ecdh_ofs || fi.Read(buf, (Bit64u)n) != (Bit64u)n) goto invalid_zip;
			for (i = n - 4; i >= 0; --i) { if (ZIP_READ_LE32(buf + i) == ZIP_END_OF_CENTRAL_DIR_HEADER_SIG) break; }
			if (i >= 0) { ecdh_ofs += i; break; }
			if (!ecdh_ofs || (fi.size - ecdh_ofs) >= (0xFFFF + ZIP_END_OF_CENTRAL_DIR_HEADER_SIZE)) goto invalid_zip;
		}

		// Read and verify the end of central directory record.
		if (fi.Seek(ecdh_ofs) != ecdh_ofs || fi.Read(buf, ZIP_END_OF_CENTRAL_DIR_HEADER_SIZE) != ZIP_END_OF_CENTRAL_DIR_HEADER_SIZE)
			goto invalid_zip;

		Bit64u total_files = ZIP_READ_LE16(buf + ZIP_ECDH_CDIR_TOTAL_ENTRIES_OFS);
		Bit64u cdir_size   = ZIP_READ_LE32(buf + ZIP_ECDH_CDIR_SIZE_OFS);
		Bit64u cdir_ofs    = ZIP_READ_LE32(buf + ZIP_ECDH_CDIR_OFS_OFS);

		if ((cdir_ofs == 0xFFFFFFFF || cdir_size == 0xFFFFFFFF || total_files == 0xFFFF)
			&& ecdh_ofs >= (ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIZE + ZIP64_END_OF_CENTRAL_DIR_HEADER_SIZE)
			&& fi.Seek(ecdh_ofs - ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIZE) == ecdh_ofs - ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIZE
			&& fi.Read(buf, ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIZE) == ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIZE
			&& ZIP_READ_LE32(buf) == ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIG)
		{
			Bit64u ecdh64_ofs = ZIP_READ_LE64(buf + ZIP64_ECDL_ECDH_OFS_OFS);
			if (ecdh64_ofs <= (fi.size - ZIP64_END_OF_CENTRAL_DIR_HEADER_SIZE)
				&& fi.Seek(ecdh64_ofs) == ecdh64_ofs
				&& fi.Read(buf, ZIP64_END_OF_CENTRAL_DIR_HEADER_SIZE) == ZIP64_END_OF_CENTRAL_DIR_HEADER_SIZE
				&& ZIP_READ_LE32(buf) == ZIP64_END_OF_CENTRAL_DIR_HEADER_SIG)
			{
				total_files = ZIP_READ_LE64(buf + ZIP64_ECDH_CDIR_TOTAL_ENTRIES_OFS);
				cdir_size   = ZIP_READ_LE64(buf + ZIP64_ECDH_CDIR_SIZE_OFS);
				cdir_ofs    = ZIP_READ_LE64(buf + ZIP64_ECDH_CDIR_OFS_OFS);
			}
		}

		if (!total_files) return true;
		if (cdir_size >= 0x10000000 // limit to 256MB content directory
			|| (cdir_size < total_files * ZIP_CENTRAL_DIR_HEADER_SIZE)
			|| ((cdir_ofs + cdir_size) > fi.size)
			) goto invalid_zip;

		Bit8u* m_central_dir = (Bit8u*)malloc((size_t)cdir_size);
		if (fi.Seek(cdir_ofs) != cdir_ofs || fi.Read(m_central_dir, cdir_size) != cdir_size)
			goto invalid_zip;
		const Bit8u *cdir_start = (const Bit8u*)m_central_dir, *cdir_end = cdir_start + cdir_size, *p = cdir_start;

		ZipReader* reader = new ZipReader(fi); // pass already opened file

		// Now create an index into the central directory file records, do some basic sanity checking on each record, and check for zip64 entries (which are not yet supported).
		p = cdir_start;
		size_t old_files_count = files.size();
		for (Bit32u i = 0, total_header_size; i < total_files && p >= cdir_start && p < cdir_end && ZIP_READ_LE32(p) == ZIP_CENTRAL_DIR_HEADER_SIG; i++, p += total_header_size)
		{
			Bit32u bit_flag         = ZIP_READ_LE16(p + ZIP_CDH_BIT_FLAG_OFS);
			Bit32u method           = ZIP_READ_LE16(p + ZIP_CDH_METHOD_OFS);
			Bit16u file_time        = ZIP_READ_LE16(p + ZIP_CDH_FILE_TIME_OFS);
			Bit16u file_date        = ZIP_READ_LE16(p + ZIP_CDH_FILE_DATE_OFS);
			Bit64u comp_size        = ZIP_READ_LE32(p + ZIP_CDH_COMPRESSED_SIZE_OFS);
			Bit64u decomp_size      = ZIP_READ_LE32(p + ZIP_CDH_DECOMPRESSED_SIZE_OFS);
			Bit32u filename_len     = ZIP_READ_LE16(p + ZIP_CDH_FILENAME_LEN_OFS);
			Bit32s extra_len        = ZIP_READ_LE16(p + ZIP_CDH_EXTRA_LEN_OFS);
			Bit64u local_header_ofs = ZIP_READ_LE32(p + ZIP_CDH_LOCAL_HEADER_OFS);
			total_header_size = ZIP_CENTRAL_DIR_HEADER_SIZE + filename_len + extra_len + ZIP_READ_LE16(p + ZIP_CDH_COMMENT_LEN_OFS);

			if (!MethodSupported(method)
				|| (p + total_header_size > cdir_end)
				|| (bit_flag & (1 | 32)) // Encryption and patch files are not supported.
				) continue;

			if (decomp_size == 0xFFFFFFFF || comp_size == 0xFFFFFFFF || local_header_ofs == 0xFFFFFFFF)
			{
				for (const Bit8u *x = p + ZIP_CENTRAL_DIR_HEADER_SIZE + filename_len, *xEnd = x + extra_len; (x + (sizeof(Bit16u) * 2)) < xEnd;)
				{
					const Bit8u *field = x + (sizeof(Bit16u) * 2), *fieldEnd = field + ZIP_READ_LE16(x + 2);
					if (ZIP_READ_LE16(x) != 0x0001 || fieldEnd > xEnd) { x = fieldEnd; continue; } // Not Zip64 extended information extra field
					if (decomp_size == 0xFFFFFFFF)
					{
						if ((size_t)(fieldEnd - field) < sizeof(Bit64u)) goto invalid_zip;
						decomp_size = ZIP_READ_LE64(field);
						field += sizeof(Bit64u);
					}
					if (comp_size == 0xFFFFFFFF)
					{
						if ((size_t)(fieldEnd - field) < sizeof(Bit64u)) goto invalid_zip;
						comp_size = ZIP_READ_LE64(field);
						field += sizeof(Bit64u);
					}
					if (local_header_ofs == 0xFFFFFFFF)
					{
						if ((size_t)(fieldEnd - field) < sizeof(Bit64u)) goto invalid_zip;
						local_header_ofs = ZIP_READ_LE64(field);
						field += sizeof(Bit64u);
					}
					break;
				}
			}

			if (((!method) && (decomp_size != comp_size)) || (decomp_size && !comp_size)
				|| (decomp_size > 0xFFFFFFFF) || (comp_size > 0xFFFFFFFF) // not supported on DOS file systems
				|| ((local_header_ofs + ZIP_LOCAL_DIR_HEADER_SIZE + comp_size) > fi.size)
				) continue;

			char *name = (char*)(p + ZIP_CENTRAL_DIR_HEADER_SIZE);
			for (char *q = name, *name_end = name + filename_len; q != name_end; q++)
				if (*q == '\\')
					*q = '/'; // convert back-slashes to regular slashes

			files.push_back(new SFileZip(*reader, name, filename_len, local_header_ofs, (Bit32u)comp_size, decomp_size, file_date, file_time, (Bit8u)bit_flag, (Bit8u)method));
		}
		free(m_central_dir);
		if (old_files_count == files.size()) delete reader;
		return true;
	}
};

struct ISOGenerator
{
	// ISO record structures (recordLen is always zero padded to an even number of bytes)
	struct PathRecord
	{
		PathRecord() {}
		Bit8u nameLen, extAttrLen, dirSector[4], parentNumber[2], name[222];
		Bit32u _parentPathIndex, _DirRecordStart, _DirRecordEnd, _DirRecordSectors, _DirSector;
		inline Bit8u RecordLen() { return (Bit8u)(8 + ((nameLen + 1) & ~1)); } // pad to even byte numbers
		inline void Pad() { if (nameLen & 1) name[nameLen] = '\0'; } // pad to even byte numbers
	};
	struct DirRecord
	{
		enum : Bit8u { FLAG_DIR = 2, RECORD_LEN_BASE = 33 };
		Bit8u recordLen, extAttrLen, dataSector[8], dataSize[8], dateY, dateM, dateD, timeH, timeM, timeS, timeZone, fileFlags, fileUnitSize, interleaveGapSize, VolumeSeqNumber[4], nameLen, name[222];
		Bit32u _fileIndex, _pathIndex;
		DirRecord() {}
		DirRecord(SFile* sf, Bit32u fileIndex, Bit32u pathIndex, bool isDir, const char* _name, Bit8u _nameLen) : _fileIndex(fileIndex), _pathIndex(pathIndex)
		{
			recordLen = RECORD_LEN_BASE + _nameLen + (1 - (_nameLen & 1)); // pad to even byte numbers
			extAttrLen = 0;
			Bit32u siz = (Bit32u)(sf ? sf->size : 0);
			ZIP_WRITE_LB32(dataSize, siz);
			SetDate((sf ? sf->date : 0), (sf ? sf->time : 0));
			timeZone = 0; // gmtOffset;
			fileFlags = (isDir ? FLAG_DIR : 0); // file flags
			fileUnitSize = 0; // interleaved mode file unit size;
			interleaveGapSize = 0; // Interleave gap size
			ZIP_WRITE_LB16(VolumeSeqNumber, 1); // volume sequence number
			nameLen = _nameLen;
			memcpy(name, _name, _nameLen);
			if (!(_nameLen & 1)) name[_nameLen] = '\0'; // pad
		}
		void SetDate(Bit16u dat, Bit16u tim) { dateY = (Bit8u)((dat>>9)+80); dateM = (Bit8u)((dat>>5)&0xf); dateD = (Bit8u)(dat&0x1f); timeH = (Bit8u)(tim>>11); timeM = (Bit8u)((tim>>5)&0x3f); timeS = (Bit8u)((tim&0x1f)*2); }
	};

	enum { CHD_V5_HEADER_SIZE = 124, CHD_V5_UNCOMPMAPENTRYBYTES = 4, CHD_METADATA_HEADER_SIZE = 16, CHD_CDROM_TRACK_METADATA2_TAG = 1128813618, CHD_CD_TRACK_PADDING = 4 };
	enum { CD_MAX_SECTOR_DATA = 2352, CD_MAX_SUBCODE_DATA = 96, CD_FRAME_SIZE = CD_MAX_SECTOR_DATA + CD_MAX_SUBCODE_DATA, ISO_FRAMESIZE = 2048 };
	enum { CHD_UNITBYTES = CD_FRAME_SIZE, CHD_HUNKBYTES = CHD_UNITBYTES * 8 };

	// Make CHD and BIN files with just 2048 byte sized sectors, also supports making .ISO files
	#define ISOGEN_SECTOR_SIZE 2048

	// Make CHD and BIN files with raw sectors including sync head and checksums, disables support for making .ISO files
	//#define ISOGEN_SECTOR_SIZE 2352

	Bit32u pathTableSize = 0, fileSectors = 0, dirRecordSectors = 0, numSectors = 0;
	std::vector<PathRecord> pathRecords; // filled LE, converted to BE during output
	std::vector<DirRecord> dirRecords;
	Bit8u sec[ISO_FRAMESIZE], *chdbuf = NULL, *chdmap = NULL, *chdhunk = NULL, *chdhunkofs;
	Bit32u chdhunkcount = 0;
	FILE* fiso;
	enum EMode { MODE_CHD, MODE_BIN, MODE_CUE, MODE_ISO } mode;
	~ISOGenerator() { free(chdbuf); }

	void ChdStartOutput(Bit32u totalISOSectors)
	{
		const Bit32u totalchdunits = (totalISOSectors + CHD_CD_TRACK_PADDING - 1) / CHD_CD_TRACK_PADDING * CHD_CD_TRACK_PADDING;
		const Bit32u totalunmappedhunks = (totalchdunits + 7) / 8;
		const Bit64u chdHeadSize = // uncompressed chd file structure
			CHD_V5_HEADER_SIZE // 124 bytes header
			+ (totalunmappedhunks * CHD_V5_UNCOMPMAPENTRYBYTES) // hunk map (4 bytes index, no entry can be 0 because 0 is the hunk with headers and hunkmap and meta info)
			+ (CHD_METADATA_HEADER_SIZE + 256); // single data track meta array, followed by zeros until next hunk boundary
		chdbuf = (Bit8u*)malloc(((chdHeadSize + 7) & ~7) + CHD_HUNKBYTES);
		chdmap = chdbuf + CHD_V5_HEADER_SIZE;
		chdhunk = chdbuf + ((chdHeadSize + 7) & ~7);
		Bit8u *rawheader = chdbuf, *meta = rawheader + CHD_V5_HEADER_SIZE + (totalunmappedhunks * CHD_V5_UNCOMPMAPENTRYBYTES);

		memcpy(&rawheader[0], "MComprHD", 8);
		ZIP_WRITE_BE32(&rawheader[8], CHD_V5_HEADER_SIZE);
		ZIP_WRITE_BE32(&rawheader[12], 5);
		memset(&rawheader[16], 0, 32 - 16);
		ZIP_WRITE_BE64(&rawheader[32], totalchdunits * CHD_UNITBYTES);
		ZIP_WRITE_BE64(&rawheader[40], CHD_V5_HEADER_SIZE); // mapoffset
		ZIP_WRITE_BE64(&rawheader[48], (Bit64u)(meta - rawheader)); //should be (mapoffset + hunkcount * 4) where hunkcount is ((logicalbytes + hunkbytes - 1) / hunkbytes)
		ZIP_WRITE_BE32(&rawheader[56], CHD_HUNKBYTES); // hunkbytes (8 units)
		ZIP_WRITE_BE32(&rawheader[60], CHD_UNITBYTES); // unitbytes
		memset(&rawheader[64], 0, CHD_V5_HEADER_SIZE - 64);
		
		int len = sprintf((char*)meta + CHD_METADATA_HEADER_SIZE, "TRACK:%d TYPE:%s SUBTYPE:%s FRAMES:%d PREGAP:%d PGTYPE:%s PGSUB:%s POSTGAP:%d",
			1, (ISOGEN_SECTOR_SIZE == 2048 ? "MODE1" : "MODE1_RAW"), "NONE", (int)totalISOSectors, 0, "MODE1", "NONE", 0);

		ZIP_WRITE_BE32(&meta[0], CHD_CDROM_TRACK_METADATA2_TAG);
		ZIP_WRITE_BE32(&meta[4], len + 1); // len of formatted string with \0 terminator
		meta[4] = 0x1; // ALWAYS 0x01 (defined as CHD_MDFLAGS_CHECKSUM)
		ZIP_WRITE_BE64(&meta[8], 0); // offset from file start to next meta element, 0 for last element

		const size_t finalHeadLen = (meta + CHD_METADATA_HEADER_SIZE + len + 1 - chdbuf);
		fwrite(chdbuf, finalHeadLen, 1, fiso);
		memset(chdhunk, 0, CHD_HUNKBYTES);
		fwrite(chdhunk, CHD_HUNKBYTES - (finalHeadLen % CHD_HUNKBYTES), 1, fiso);
		chdhunkofs = chdhunk;
		chdhunkcount = (Bit32u)((finalHeadLen + CHD_HUNKBYTES - 1) / CHD_HUNKBYTES);
	}

	void ChdEndOutput()
	{
		if (mode != MODE_CHD) return;

		const size_t totalunmappedhunks = ((chdmap - (chdbuf + CHD_V5_HEADER_SIZE)) / 4) + ((chdhunkofs != chdhunk) ? 1 : 0);
		if (Bit32u garbagehunkidx = ((chdhunkofs != chdhunk && chdhunkofs <= chdhunk + (CHD_HUNKBYTES / 2) && totalunmappedhunks > 256) ? ZIP_READ_BE32(chdmap - 256*4) : 0))
		{
			// An unintended (but consistent) behavior of chdman can add garbage at the end of the final chunk from 256 (mapped) hunks ago
			memset(chdhunkofs, 0, (chdhunk + (CHD_HUNKBYTES / 2) - chdhunkofs));
			fseek_wrap(fiso, (garbagehunkidx * CHD_HUNKBYTES + (CHD_HUNKBYTES / 2)), SEEK_SET);
			fread(chdhunk + (CHD_HUNKBYTES / 2), (CHD_HUNKBYTES / 2), 1, fiso);
			fseek_wrap(fiso, 0, SEEK_END);
			ChdPutHunk();
		}
		else if (chdhunkofs != chdhunk)
		{
			memset(chdhunkofs, 0, (chdhunk + CHD_HUNKBYTES - chdhunkofs));
			ChdPutHunk();
		}
		ZIP_ASSERT(ZIP_READ_BE32(chdmap) == CHD_CDROM_TRACK_METADATA2_TAG); // chdmap should now be perfectly filled and the cursor ends up being where the first track meta tag is
		fseek(fiso, 0, SEEK_SET);
		fwrite(chdbuf, chdmap - chdbuf, 1, fiso); // write hunk map
	}

	void ChdPutHunk()
	{
		bool allzeros = true;
		for (Bit64u* p = (Bit64u*)chdhunk, *pEnd = (Bit64u*)(chdhunk + CHD_HUNKBYTES); p != pEnd; p++) { if (*p) { allzeros = false; break; } }
		if (allzeros)
		{
			ZIP_WRITE_BE32(chdmap, 0);
			chdmap += 4;
		}
		else
		{
			ZIP_WRITE_BE32(chdmap, chdhunkcount);
			chdmap += 4;
			fwrite(chdhunk, CHD_HUNKBYTES, 1, fiso);
			chdhunkcount++;
		}
		chdhunkofs = chdhunk;
	}

	void AddSector(size_t len)
	{
		ZIP_ASSERT(len <= ISO_FRAMESIZE);
		memset(sec + len, 0, ISO_FRAMESIZE - len);
		#if ISOGEN_SECTOR_SIZE == 2048 // ISO with 2048 bytes per sector
		if (mode == MODE_CHD) { memcpy(chdhunkofs, sec, ISO_FRAMESIZE); if ((chdhunkofs += CHD_UNITBYTES) == (chdhunk + CHD_HUNKBYTES)) ChdPutHunk(); }
		else fwrite(sec, ISO_FRAMESIZE, 1, fiso);
		#elif ISOGEN_SECTOR_SIZE == 2352 // BIN with 2352 bytes per sector, MODE1 Data Track
		// MODE1 sector generation based on ECM by Neill Corlett
		Bit8u head[16], tail[288];
		// [0x000 ~ 0x00B] Sync pattern (00 FF ... FF 00)
		head[0] = 0x00;
		memset(head + 0x001, 0xFF, 0x00A);
		head[0x00B] = 0x00;
		// [0x00C ~ 0x00E] Sector address in MSF format (decimal number in hex)
		Bit32u nsector = numSectors + (75 * 2);
		Bit8u addrM = (nsector / (75 * 60)), addrS =  ((nsector / 75) % 60), addrF = (nsector % 75);
		head[0x00C] = ((addrM / 10 * 0x10) | (addrM % 10));
		head[0x00D] = ((addrS / 10 * 0x10) | (addrS % 10));
		head[0x00E] = ((addrF / 10 * 0x10) | (addrF % 10));
		// [0x00F] Data Track Mode
		head[0x00F] = 0x01;
		// EDC and ECC lookup tables
		static Bit8u ecc_f_lut[256], ecc_b_lut[256];
		static Bit32u edc_lut[256];
		if (!edc_lut[1]) 
		{
			for (Bit32u i = 0; i < 256; i++)
			{
				Bit8u j = (Bit8u)((i << 1) ^ (i & 0x80 ? 0x11D : 0));
				ecc_f_lut[i] = j;
				ecc_b_lut[i ^ j] = i;
				Bit32u edc = i;
				for (j = 0; j < 8; j++) edc = (edc >> 1) ^ (edc & 1 ? 0xD8018001 : 0);
				edc_lut[i] = edc;
			}
		}
		// [0x810 ~ 0x813] EDC (4 byte checksum)
		Bit32u edc = 0;
		//for (Bit8u *edcsrc = sector, *edcend = edcsrc + 0x810; edcsrc != edcend;) edc = (edc >> 8) ^ edc_lut[(edc ^ *(edcsrc++)) & 0xFF];
		for (Bit8u *edcsrc = head, *edcend = head + 0x010; edcsrc != edcend;) edc = (edc >> 8) ^ edc_lut[(edc ^ *(edcsrc++)) & 0xFF];
		for (Bit8u *edcsrc = sec,  *edcend = sec  + 0x800; edcsrc != edcend;) edc = (edc >> 8) ^ edc_lut[(edc ^ *(edcsrc++)) & 0xFF];
		ZIP_WRITE_LE32(tail, edc);
		// [0x814 ~ 0x81B] Reserved (zeroed)
		memset(tail + 0x004, 0x00, 8);
		// [0x81C ~ 0x92F] ECC (Error correction codes)
		Bit8u *eccout = tail+0x00C;
		const size_t major_counts[] = { 86, 52 }, minor_counts[] = { 24, 43 }, major_mults[] = { 2, 86 }, minor_incs[] = { 86, 88 };
		for (int pq = 0; pq != 2; pq++, eccout += 0xAC) // first P then Q codes 0xAC bytes afterwards
		{
			const size_t major_count = major_counts[pq], minor_count = minor_counts[pq], major_mult = major_mults[pq], minor_inc = minor_incs[pq];
			for (size_t size = major_count * minor_count, major = 0; major < major_count; major++)
			{
				Bit8u ecc_a = 0, ecc_b = 0;
				for (size_t index = (major >> 1) * major_mult + (major & 1), minor = 0; minor < minor_count; minor++)
				{
					const Bit8u temp = (index < 4 ? head[0x00C + index] : (index < 2052 ? sec[index - 4] : tail[index - 2052]));
					index += minor_inc;
					if (index >= size) { index -= size; }
					ecc_b ^= temp;
					ecc_a = ecc_f_lut[ecc_a ^ temp];
				}
				ecc_a = ecc_b_lut[ecc_f_lut[ecc_a] ^ ecc_b];
				eccout[major              ] = (ecc_a        );
				eccout[major + major_count] = (ecc_a ^ ecc_b);
			}
		}
		if (mode == MODE_CHD)
		{
			memcpy(chdhunkofs + 0x000, head, sizeof(head)); // [0x000 ~ 0x00F] Head
			memcpy(chdhunkofs + 0x010, sec,  sizeof(sec )); // [0x010 ~ 0x80F] Data
			memcpy(chdhunkofs + 0x810, tail, sizeof(tail)); // [0x810 ~ 0x92F] Tail
			if ((chdhunkofs += CHD_UNITBYTES) == (chdhunk + CHD_HUNKBYTES)) ChdPutHunk();
		}
		else
		{
			fwrite(head, sizeof(head), 1, fiso); // [0x000 ~ 0x00F] Head
			fwrite(sec,  sizeof(sec ), 1, fiso); // [0x010 ~ 0x80F] Data
			fwrite(tail, sizeof(tail), 1, fiso); // [0x810 ~ 0x92F] Tail
		}
		#else
		#error Not implemented
		#endif
		numSectors++;
	}

	void FillDir(SFile** sf, SFile** sfBegin, SFile** sfEnd, size_t baseLen, Bit8u dirNameLen = 0, const DirRecord* parentDot = NULL, Bit32u parentPathIndex = 0, Bit32u parentDirRecordIndex = (Bit32u)-1)
	{
		ZIP_ASSERT(!sf || ((*sf)->path[baseLen] == '/' && (*sf)->path[baseLen-1] != '/' && (*sf)->path[baseLen+1] != '/'));
		//if (sf) printf("DIR [%8.*s]/[%-8.*s] (First File:  %s)\n", (parentDot ? pathRecords[parentDot->_pathIndex].nameLen : 0), (parentDot ? (char*)pathRecords[parentDot->_pathIndex].name : ""), dirNameLen, (*sf)->path.c_str() + baseLen - dirNameLen, (*sf)->path.c_str());

		PathRecord pr;
		pr.nameLen = (dirNameLen ? dirNameLen : 1);
		pr.extAttrLen = 0;
		memcpy(pr.name, (dirNameLen ? (*sf)->path.c_str() + baseLen - dirNameLen : "\0"), pr.nameLen);
		pr.Pad();
		pathTableSize += pr.RecordLen();
		pr._parentPathIndex = parentPathIndex;
		pr._DirRecordStart = (Bit32u)dirRecords.size();

		SFile* sfMaxDate = NULL; // use highest time stamp of a recursively contained file (ignore unreliable directory timestamps)
		Bit32u dirSectors = 1, dirBytesLeft = (ISO_FRAMESIZE - ((DirRecord::RECORD_LEN_BASE + 1) * 2)); // dot and dotdot records already used
		dirRecords.resize(dirRecords.size() + 2);
		if (sf) for (const char *curName, *curNameEnd, *lastName = NULL, *lastNameEnd = NULL;; lastName = curName, lastNameEnd = curNameEnd)
		{
			if ((!sfMaxDate || ((*sf)->date > sfMaxDate->date || ((*sf)->date == sfMaxDate->date && (*sf)->time > sfMaxDate->time))) && (*sf)->path.back() != '/') sfMaxDate = *sf;
			const char *sfPath = (*sf)->path.c_str();
			curName = sfPath + baseLen + 1;
			const char *dirTerm = strchr(curName, '/');
			curNameEnd = (dirTerm ? dirTerm : curName + strlen(curName));
			size_t nameLen = (curNameEnd - curName);
			if (nameLen && nameLen < sizeof(((DirRecord*)sf)->name) && (nameLen != (size_t)(lastNameEnd - lastName) || memcmp(curName, lastName, nameLen)))
			{
				//printf("    %s [%.*s] Size: %u\n", (!!dirTerm ? "SDIR" : "FILE"), nameLen, curName, (*sf)->size);
				dirRecords.emplace_back(*sf, (Bit32u)(sf - sfBegin), (Bit32u)-1, !!dirTerm, curName, (Bit8u)nameLen);
				if (!dirTerm) fileSectors += (Bit32u)((*sf)->size + (ISO_FRAMESIZE-1)) / ISO_FRAMESIZE;
				Bit32u drLen = dirRecords.back().recordLen;
				if (dirBytesLeft < drLen) { dirSectors++; dirBytesLeft = ISO_FRAMESIZE - drLen; } else dirBytesLeft -= drLen;
			}
			if (++sf == sfEnd || (*sf)->path.length() <= baseLen || memcmp((*sf)->path.c_str(), sfPath, baseLen + 1)) break;
		}

		Bit32u pathIndex = (Bit32u)pathRecords.size();
		pr._DirRecordEnd = (Bit32u)dirRecords.size();
		pr._DirRecordSectors = dirSectors;
		pathRecords.push_back(pr);
		dirRecordSectors += dirSectors;

		DirRecord drDot(sfMaxDate, (Bit32u)-1, pathIndex, true, "\0", (Bit8u)1);
		dirRecords[pr._DirRecordStart + 0] = drDot;
		dirRecords[pr._DirRecordStart + 1] = (parentDot ? *parentDot : drDot);
		dirRecords[pr._DirRecordStart + 1].name[0] = '\x01';
		if (parentDot) dirRecords[parentDirRecordIndex].SetDate((sfMaxDate ? sfMaxDate->date : 0), (sfMaxDate ? sfMaxDate->time : 0));

		for (Bit32u i = pr._DirRecordStart + 2, iEnd = pr._DirRecordEnd; i != iEnd; i++)
		{
			DirRecord& dr = dirRecords[i];
			if (!(dr.fileFlags & DirRecord::FLAG_DIR)) continue;
			dr._pathIndex = (Bit32u)pathRecords.size();
			FillDir(sfBegin + dr._fileIndex , sfBegin, sfEnd, baseLen + 1 + dr.nameLen, dr.nameLen, &drDot, pathIndex, i);
		}
	}

	void DoOutput(std::vector<SFile*>& files, const char* label = NULL)
	{
		const Bit32u lePathTableSector = 16 + 1 + 1;
		const Bit32u pathTableSectors = ((pathTableSize + (ISO_FRAMESIZE-1)) / ISO_FRAMESIZE);
		const Bit32u bePathTableSector = lePathTableSector + pathTableSectors;
		const Bit32u firstDirRecordSector = bePathTableSector + pathTableSectors;
		const Bit32u firstFileDataSector = firstDirRecordSector + dirRecordSectors;
		const Bit32u totalISOSectors = firstFileDataSector + fileSectors;
		Log("Got %u files in %u directories which will result in a %u MB size ISO\n", (unsigned)files.size(), (unsigned)pathRecords.size(), (unsigned)(totalISOSectors * 2048 / 1024 / 1024));

		// 16 boot sectors, volume descriptor, volume terminator
		Log("Writing header sectors...\n");
		if (mode == MODE_CHD) ChdStartOutput(totalISOSectors);
		for (int i = 0; i != 16; i++) AddSector(0);
		sec[0] = 1; // type primary volume
		memcpy(sec+1, "CD001", 5); // id
		sec[6] = 1; // version
		sec[7] = 0; // unused
		memset(sec+8, ' ', 32); // system id string
		memset(sec+40, ' ', 32); // volume id string
		if (label) { size_t llen = strlen(label); memcpy(sec+40, label, (llen > 32 ? 32 : llen)); }
		memset(sec+72, 0, 8); // unused
		ZIP_WRITE_LB32(sec+80, totalISOSectors) // total logical sectors
		memset(sec+88, 0, 32); // unused
		ZIP_WRITE_LB16(sec+120, 1) // set number
		ZIP_WRITE_LB16(sec+124, 1) // sequence number
		ZIP_WRITE_LB16(sec+128, ISO_FRAMESIZE) // logical block size
		ZIP_WRITE_LB32(sec+132, pathTableSize) // path table size
		ZIP_WRITE_LE32(sec+140, lePathTableSector) // path table sector le
		ZIP_WRITE_LE32(sec+144, 0) // path table opt le
		ZIP_WRITE_BE32(sec+148, bePathTableSector) // path table sector be
		ZIP_WRITE_BE32(sec+152, 0) // path table opt be
		ZIP_ASSERT(dirRecords[0].recordLen == 34 && (dirRecords[0].fileFlags & DirRecord::FLAG_DIR) && dirRecords[0].name[0] == '\0');
		ZIP_WRITE_LB32(dirRecords[0].dataSector, firstDirRecordSector);
		ZIP_WRITE_LB32(dirRecords[0].dataSize, pathRecords[0]._DirRecordSectors * ISO_FRAMESIZE);
		memcpy(sec+156, &dirRecords[0], DirRecord::RECORD_LEN_BASE + 1);
		memset(sec+190, ' ', 128); // set id
		memset(sec+318, ' ', 128); // publisher id
		memset(sec+446, ' ', 128); // preparer id
		memset(sec+574, ' ', 128); // application id
		memset(sec+702, ' ', 37); // copyright file id
		memset(sec+739, ' ', 37); // abstract file id
		memset(sec+776, ' ', 37); // bibliographic file id
		memcpy(sec+813, "0000000000000000\0", 17); // creation time
		memcpy(sec+830, "0000000000000000\0", 17); // modification time
		memcpy(sec+847, "0000000000000000\0", 17); // expiration time
		memcpy(sec+864, "0000000000000000\0", 17); // effective time
		sec[881] = 1; // file struct version
		AddSector(882);
		sec[0] = 255; // type terminator
		memcpy(sec+1, "CD001", 5); // id
		sec[6] = 1; // version
		AddSector(7);

		PathRecord *prBegin = &pathRecords[0], *prEnd = prBegin + pathRecords.size();
		Bit32u dirSec = firstDirRecordSector;
		for (PathRecord* pr = prBegin; pr != prEnd; dirSec += (pr++)->_DirRecordSectors) pr->_DirSector = dirSec;
		ZIP_ASSERT(dirSec == firstFileDataSector);

		Log("Writing %u path records ...\n", (unsigned)pathRecords.size());
		for (Bit32u tableBE = 0; tableBE != 2; tableBE++)
		{
			ZIP_ASSERT(numSectors == (!tableBE ? lePathTableSector : bePathTableSector));
			Bit32u ofs = 0, remain = ISO_FRAMESIZE;
			for (PathRecord* pr = prBegin; pr != prEnd; remain = ISO_FRAMESIZE)
			{
				while (remain && pr != prEnd)
				{
					Bit16u parentNumber = (Bit16u)(pr->_parentPathIndex + 1);
					if (!tableBE) { ZIP_WRITE_LE32(pr->dirSector, pr->_DirSector); ZIP_WRITE_LE16(pr->parentNumber, parentNumber) }
					else          { ZIP_WRITE_BE32(pr->dirSector, pr->_DirSector); ZIP_WRITE_BE16(pr->parentNumber, parentNumber) }
					Bit32u step = (pr->RecordLen() - ofs);
					if (remain >= step) { memcpy((sec+ISO_FRAMESIZE)-remain, ((Bit8u*)pr) + ofs, step  ); ofs =      0; remain -= step; pr++; }
					else                { memcpy((sec+ISO_FRAMESIZE)-remain, ((Bit8u*)pr) + ofs, remain); ofs = remain; remain  = 0; }
				}
				AddSector(ISO_FRAMESIZE-remain);
			}
		}
		ZIP_ASSERT(numSectors == firstDirRecordSector);

		Log("Writing %u directory records ...\n", (unsigned)dirRecords.size());
		Bit32u fileSec = firstFileDataSector;
		for (PathRecord* pr = prBegin; pr != prEnd; pr++)
		{
			ZIP_ASSERT(numSectors == pr->_DirSector);
			Bit32u remain = ISO_FRAMESIZE;
			for (DirRecord *dr = &dirRecords[pr->_DirRecordStart], *drEnd = &dirRecords[0] + pr->_DirRecordEnd; dr != drEnd; remain = ISO_FRAMESIZE)
			{
				for (; remain >= dr->recordLen && dr != drEnd; remain -= (dr++)->recordLen)
				{
					if (dr->fileFlags & DirRecord::FLAG_DIR)
					{
						PathRecord* subpr = &pathRecords[dr->_pathIndex];
						ZIP_WRITE_LB32(dr->dataSector, subpr->_DirSector);
						ZIP_WRITE_LB32(dr->dataSize, subpr->_DirRecordSectors * ISO_FRAMESIZE);
					}
					else
					{
						Bit32u dataSize = ZIP_READ_LE32(dr->dataSize);
						ZIP_WRITE_LB32(dr->dataSector, (dataSize ? fileSec : 0));
						if (dataSize) fileSec += (dataSize + (ISO_FRAMESIZE-1)) / ISO_FRAMESIZE;
					}
					memcpy((sec+ISO_FRAMESIZE)-remain, dr, dr->recordLen);
				}
				AddSector(ISO_FRAMESIZE-remain);
			}
		}
		ZIP_ASSERT(numSectors == firstFileDataSector && fileSec == totalISOSectors);

		Log("Writing file contents ...\n");
		for (DirRecord *dr = &dirRecords[0], *drEnd = &dirRecords[0] + dirRecords.size(); dr != drEnd; dr++)
		{
			if ((dr->fileFlags & DirRecord::FLAG_DIR) || !ZIP_READ_LE32(dr->dataSize)) continue;
			ZIP_ASSERT(numSectors == ZIP_READ_LE32(dr->dataSector));
			SFile* fi = files[dr->_fileIndex];
			fi->Open();
			for (Bit64u step; (step = fi->Read(sec, ISO_FRAMESIZE)) != 0;) AddSector((size_t)step);
			fi->Close();
		}
		ZIP_ASSERT(numSectors == totalISOSectors);

		if (mode == MODE_CHD) ChdEndOutput();
	}

	static bool Generate(std::vector<SFile*>& files, size_t basePathLen, bool forceOverwrite, const char* outPath, const char* label = NULL)
	{
		std::string tmp;
		EMode mode;
		const char* ext = strrchr(outPath, '.'), *outCue = NULL;
		switch ((ext && ext[1]) ? ext[2]|0x20 : 0)
		{
			case 'h': mode = MODE_CHD; break;
			case 'i': mode = MODE_BIN; break;
			case 'u': mode = MODE_CUE; break;
			#if ISOGEN_SECTOR_SIZE == 2048
			case 's': mode = MODE_ISO; break;
			#endif
			default: LogErr("Invalid output file '%s', must end with CHD, BIN, CUE or ISO\n"); return false;
		}

		if (mode == MODE_BIN || mode == MODE_CUE)
		{
			const char* orgPath = outPath;
			(mode == MODE_BIN ? outCue : outPath) = PathSetFileExt(tmp.assign(outPath), (mode == MODE_BIN ? "cue" : "bin")).c_str();
			(mode == MODE_BIN ? outPath : outCue) = orgPath;
		}

		if (!forceOverwrite)
		{
			FILE* test = fopen(outPath, "rb");
			if (test) { fclose(test); LogErr("Output file '%s' already exists (specify -f to force overwriting)\n", outPath); return false; }
			if (outCue) test = fopen(outCue, "rb");
			if (test) { fclose(test); LogErr("Output file '%s' already exists (specify -f to force overwriting)\n", outCue); return false; }
		}

		FILE* ffiso = fopen_utf8(outPath, (mode == MODE_CHD ? "wb+" : "wb")); // Chd needs + for EndOutput
		if (!ffiso) { LogErr("Unable to open output file '%s' for writing\n", outPath); return false; }

		if (outCue)
		{
			FILE* fcue = fopen(outCue, "wb");
			if (!fcue) { LogErr("Unable to open output file '%s' for writing\n", outCue); return false; }
			Log("Writing cue sheet to %s ...\n", outCue);
			const char* lastS = strrchr(outPath, '/'), *lastBS = strrchr(outPath, '\\');
			fprintf(fcue, "FILE \"%s\" BINARY\r\n  TRACK 01 MODE1/%u\r\n    INDEX 01 00:00:00\r\n", (lastS > lastBS ? lastS + 1 : lastBS ? lastBS + 1 : outPath), ISOGEN_SECTOR_SIZE);
			fclose(fcue);
		}

		Log("Writing result to %s ...\n", outPath);
		SFile** sfBegin = (!files.empty() ? &files[0] : NULL);
		ISOGenerator* iso = new ISOGenerator();
		Log("Preparing CD-ROM ISO 9660 Structure ...\n");
		iso->FillDir(sfBegin, sfBegin, sfBegin + files.size(), basePathLen);
		iso->mode = mode;
		iso->fiso = ffiso;
		iso->DoOutput(files, label);
		delete iso;
		fclose(ffiso);
		Log("Done!\n\n");
		return true;
	}
};

int main(int argc, char *argv[])
{
	const char *inPath = NULL, *outPath = NULL, *label = NULL, *force = NULL;
	for (int i = 1; i < argc; i++)
	{
		if (argv[i][0] != '-' && !inPath)  { inPath  = argv[i]; continue; }
		if (argv[i][0] != '-' && !outPath) { outPath = argv[i]; continue; }
		if (argv[i][0] != '-' || !argv[i][1] || argv[i][2]) goto argerr;
		switch (argv[i][1])
		{
			case 'i': if (inPath  || ++i == argc) goto argerr; inPath  = argv[i]; continue;
			case 'o': if (outPath || ++i == argc) goto argerr; outPath = argv[i]; continue;
			case 'l': if (label   || ++i == argc) goto argerr; label   = argv[i]; continue;
			case 'f': if (force                 ) goto argerr; force   = argv[i]; continue;
		}
		argerr: LogErr("Unknown command line option '%s'.\n\n", argv[i]); goto help;
	}
	if (!inPath)
	{
		help:
		LogErr("%s v%s - Command line options:\n"
			"  [-i] <PATH>: Path to input DOSZ/ZIP file (required)\n"
			"  [-o] <PATH>: Path to output file with CHD/CUE/ISO extension (optional, defaults to input file with a CHD extension)\n"
			"  -l <LABEL> : Specify label\n"
			"  -f         : Force overwrite existing files\n"
			"\n", "DOSZtoCHD", "0.2");
		return 1;
	}

	std::string zfpath(inPath);
	SFileRaw zf(zfpath, false);
	if (!zf.size) { LogErr("Unable to open input file '%s'\n", inPath); goto help; }

	Log("Reading input file '%s' ...\n", inPath);
	std::vector<SFile*> files;
	if (!SFileZip::IndexFiles(zf, files)) { LogErr("Not a valid ZIP file '%s'\n", inPath); goto help; }

	// Sort files by filename
	struct Local
	{
		static int SortFunc(const void* va, const void* vb)
		{
			SFile *a = *(SFile**)va, *b = *(SFile**)vb;
			for (const char *pa = a->path.c_str(), *pb = b->path.c_str();;)
			{
				char ca = *(pa++), cb = *(pb++);
				int diff = ca - cb;
				if (diff == 0 && ca) continue;
				return diff;
			}
		}
	};
	if (!files.empty()) qsort(&files[0], files.size(), sizeof(files[0]), Local::SortFunc);

	std::string outPathTmp;
	if (!outPath) outPath = PathSetFileExt(outPathTmp.assign(inPath), /*"iso"*/"chd").c_str();
	ISOGenerator::Generate(files, zf.path.length(), !!force, outPath, label);

	return 0;
}

struct miniz
{
	// BASED ON MINIZ
	// miniz.c v1.15 - public domain deflate
	// Rich Geldreich <richgel99@gmail.com>, last updated Oct. 13, 2013

	// Set MINIZ_HAS_64BIT_REGISTERS to 1 if operations on 64-bit integers are reasonably fast (and don't involve compiler generated calls to helper functions).
	#if defined(_M_X64) || defined(_WIN64) || defined(__MINGW64__) || defined(_LP64) || defined(__LP64__) || defined(__ia64__) || defined(__x86_64__)
	#define MINIZ_HAS_64BIT_REGISTERS 1
	#else
	#define MINIZ_HAS_64BIT_REGISTERS 0
	#endif

	enum
	{
		// Decompression flags used by tinfl_decompress().
		TINFL_FLAG_HAS_MORE_INPUT = 2,                // If set, there are more input bytes available beyond the end of the supplied input buffer. If clear, the input buffer contains all remaining input.
		TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF = 4, // If set, the output buffer is large enough to hold the entire decompressed stream. If clear, the output buffer is at least the size of the dictionary (typically 32KB).

		// Max size of read buffer.
		MZ_ZIP_MAX_IO_BUF_SIZE = 16*1024, // Was 64*1024 originally (though max size readable through DOS_File would be 0xFFFF).

		// Max size of LZ dictionary (output buffer).
		TINFL_LZ_DICT_SIZE = 32*1024, // fixed for zip

		// Internal/private bits follow.
		TINFL_MAX_HUFF_TABLES = 3, TINFL_MAX_HUFF_SYMBOLS_0 = 288, TINFL_MAX_HUFF_SYMBOLS_1 = 32, TINFL_MAX_HUFF_SYMBOLS_2 = 19,
		TINFL_FAST_LOOKUP_BITS = 10, TINFL_FAST_LOOKUP_SIZE = 1 << TINFL_FAST_LOOKUP_BITS,

		// Number coroutine states consecutively
		TINFL_STATE_INDEX_BLOCK_BOUNDRY = 1,
		TINFL_STATE_3 , TINFL_STATE_5 , TINFL_STATE_6 , TINFL_STATE_7 , TINFL_STATE_51, TINFL_STATE_52,
		TINFL_STATE_9 , TINFL_STATE_38, TINFL_STATE_11, TINFL_STATE_14, TINFL_STATE_16, TINFL_STATE_18,
		TINFL_STATE_23, TINFL_STATE_24, TINFL_STATE_25, TINFL_STATE_26, TINFL_STATE_27, TINFL_STATE_53,
		TINFL_STATE_END
	};

	// Return status.
	enum tinfl_status
	{
		TINFL_STATUS_BAD_PARAM = -3,
		TINFL_STATUS_FAILED = -1,
		TINFL_STATUS_DONE = 0,
		TINFL_STATUS_NEEDS_MORE_INPUT = 1,
		TINFL_STATUS_HAS_MORE_OUTPUT = 2,
	};

	#if MINIZ_HAS_64BIT_REGISTERS
	typedef Bit64u tinfl_bit_buf_t;
	#else
	typedef Bit32u tinfl_bit_buf_t;
	#endif

	struct tinfl_huff_table
	{
		Bit16s m_look_up[TINFL_FAST_LOOKUP_SIZE];
		Bit16s m_tree[TINFL_MAX_HUFF_SYMBOLS_0 * 2];
		Bit8u m_code_size[TINFL_MAX_HUFF_SYMBOLS_0];
	};

	struct tinfl_decompressor
	{
		tinfl_huff_table m_tables[TINFL_MAX_HUFF_TABLES];
		Bit32u m_state, m_num_bits, m_final, m_type, m_dist, m_counter, m_num_extra, m_table_sizes[TINFL_MAX_HUFF_TABLES];
		tinfl_bit_buf_t m_bit_buf;
		size_t m_dist_from_out_buf_start;
		Bit8u m_raw_header[4], m_len_codes[TINFL_MAX_HUFF_SYMBOLS_0 + TINFL_MAX_HUFF_SYMBOLS_1 + 137];
	};

	// Initializes the decompressor to its initial state.
	static void tinfl_init(tinfl_decompressor *r) { r->m_state = 0; }

	// Main low-level decompressor coroutine function. This is the only function actually needed for decompression. All the other functions are just high-level helpers for improved usability.
	// This is a universal API, i.e. it can be used as a building block to build any desired higher level decompression API. In the limit case, it can be called once per every byte input or output.
	static tinfl_status tinfl_decompress(tinfl_decompressor *r, const Bit8u *pIn_buf_next, Bit32u *pIn_buf_size, Bit8u *pOut_buf_start, Bit8u *pOut_buf_next, Bit32u *pOut_buf_size, const Bit32u decomp_flags)
	{
		// An attempt to work around MSVC's spammy "warning C4127: conditional expression is constant" message.
		#ifdef _MSC_VER
		#define TINFL_MACRO_END while (0, 0)
		#else
		#define TINFL_MACRO_END while (0)
		#endif

		#define TINFL_MEMCPY(d, s, l) memcpy(d, s, l)
		#define TINFL_MEMSET(p, c, l) memset(p, c, l)
		#define TINFL_CLEAR(obj) memset(&(obj), 0, sizeof(obj))

		#define TINFL_CR_BEGIN switch(r->m_state) { case 0:
		#define TINFL_CR_RETURN(state_index, result) do { status = result; r->m_state = state_index; goto common_exit; case state_index:; } TINFL_MACRO_END
		#define TINFL_CR_RETURN_FOREVER(state_index, result) do { status = result; r->m_state = TINFL_STATE_END; goto common_exit; } TINFL_MACRO_END
		#define TINFL_CR_FINISH }

		// TODO: If the caller has indicated that there's no more input, and we attempt to read beyond the input buf, then something is wrong with the input because the inflator never
		// reads ahead more than it needs to. Currently TINFL_GET_BYTE() pads the end of the stream with 0's in this scenario.
		#define TINFL_GET_BYTE(state_index, c) do { \
			if (pIn_buf_cur >= pIn_buf_end) { \
				for ( ; ; ) { \
					if (decomp_flags & TINFL_FLAG_HAS_MORE_INPUT) { \
						TINFL_CR_RETURN(state_index, TINFL_STATUS_NEEDS_MORE_INPUT); \
						if (pIn_buf_cur < pIn_buf_end) { \
							c = *pIn_buf_cur++; \
							break; \
						} \
					} else { \
						c = 0; \
						break; \
					} \
				} \
			} else c = *pIn_buf_cur++; } TINFL_MACRO_END

		#define TINFL_NEED_BITS(state_index, n) do { Bit32u c; TINFL_GET_BYTE(state_index, c); bit_buf |= (((tinfl_bit_buf_t)c) << num_bits); num_bits += 8; } while (num_bits < (Bit32u)(n))
		#define TINFL_SKIP_BITS(state_index, n) do { if (num_bits < (Bit32u)(n)) { TINFL_NEED_BITS(state_index, n); } bit_buf >>= (n); num_bits -= (n); } TINFL_MACRO_END
		#define TINFL_GET_BITS(state_index, b, n) do { if (num_bits < (Bit32u)(n)) { TINFL_NEED_BITS(state_index, n); } b = bit_buf & ((1 << (n)) - 1); bit_buf >>= (n); num_bits -= (n); } TINFL_MACRO_END

		// TINFL_HUFF_BITBUF_FILL() is only used rarely, when the number of bytes remaining in the input buffer falls below 2.
		// It reads just enough bytes from the input stream that are needed to decode the next Huffman code (and absolutely no more). It works by trying to fully decode a
		// Huffman code by using whatever bits are currently present in the bit buffer. If this fails, it reads another byte, and tries again until it succeeds or until the
		// bit buffer contains >=15 bits (deflate's max. Huffman code size).
		#define TINFL_HUFF_BITBUF_FILL(state_index, pHuff) \
			do { \
				temp = (pHuff)->m_look_up[bit_buf & (TINFL_FAST_LOOKUP_SIZE - 1)]; \
				if (temp >= 0) { \
					code_len = temp >> 9; \
					if ((code_len) && (num_bits >= code_len)) \
					break; \
				} else if (num_bits > TINFL_FAST_LOOKUP_BITS) { \
					 code_len = TINFL_FAST_LOOKUP_BITS; \
					 do { \
							temp = (pHuff)->m_tree[~temp + ((bit_buf >> code_len++) & 1)]; \
					 } while ((temp < 0) && (num_bits >= (code_len + 1))); if (temp >= 0) break; \
				} TINFL_GET_BYTE(state_index, c); bit_buf |= (((tinfl_bit_buf_t)c) << num_bits); num_bits += 8; \
			} while (num_bits < 15);

		// TINFL_HUFF_DECODE() decodes the next Huffman coded symbol. It's more complex than you would initially expect because the zlib API expects the decompressor to never read
		// beyond the final byte of the deflate stream. (In other words, when this macro wants to read another byte from the input, it REALLY needs another byte in order to fully
		// decode the next Huffman code.) Handling this properly is particularly important on raw deflate (non-zlib) streams, which aren't followed by a byte aligned adler-32.
		// The slow path is only executed at the very end of the input buffer.
		#define TINFL_HUFF_DECODE(state_index, sym, pHuff) do { \
			int temp; Bit32u code_len, c; \
			if (num_bits < 15) { \
				if ((pIn_buf_end - pIn_buf_cur) < 2) { \
					 TINFL_HUFF_BITBUF_FILL(state_index, pHuff); \
				} else { \
					 bit_buf |= (((tinfl_bit_buf_t)pIn_buf_cur[0]) << num_bits) | (((tinfl_bit_buf_t)pIn_buf_cur[1]) << (num_bits + 8)); pIn_buf_cur += 2; num_bits += 16; \
				} \
			} \
			if ((temp = (pHuff)->m_look_up[bit_buf & (TINFL_FAST_LOOKUP_SIZE - 1)]) >= 0) \
				code_len = temp >> 9, temp &= 511; \
			else { \
				code_len = TINFL_FAST_LOOKUP_BITS; do { temp = (pHuff)->m_tree[~temp + ((bit_buf >> code_len++) & 1)]; } while (temp < 0); \
			} sym = temp; bit_buf >>= code_len; num_bits -= code_len; } TINFL_MACRO_END

		static const int s_length_base[31] = { 3,4,5,6,7,8,9,10,11,13, 15,17,19,23,27,31,35,43,51,59, 67,83,99,115,131,163,195,227,258,0,0 };
		static const int s_length_extra[31]= { 0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,0,0 };
		static const int s_dist_base[32] = { 1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193, 257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577,0,0};
		static const int s_dist_extra[32] = { 0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13};
		static const Bit8u s_length_dezigzag[19] = { 16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15 };
		static const int s_min_table_sizes[3] = { 257, 1, 4 };

		tinfl_status status = TINFL_STATUS_FAILED; Bit32u num_bits, dist, counter, num_extra; tinfl_bit_buf_t bit_buf;
		const Bit8u *pIn_buf_cur = pIn_buf_next, *const pIn_buf_end = pIn_buf_next + *pIn_buf_size, *const pIn_buf_end_m_4 = pIn_buf_end - 4;
		Bit8u *pOut_buf_cur = pOut_buf_next, *const pOut_buf_end = pOut_buf_next + *pOut_buf_size, *const pOut_buf_end_m_2 = pOut_buf_end - 2;
		size_t out_buf_size_mask = (decomp_flags & TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF) ? (size_t)-1 : ((pOut_buf_next - pOut_buf_start) + *pOut_buf_size) - 1, dist_from_out_buf_start;

		Bit16s* r_tables_0_look_up = r->m_tables[0].m_look_up;

		// Ensure the output buffer's size is a power of 2, unless the output buffer is large enough to hold the entire output file (in which case it doesn't matter).
		if (((out_buf_size_mask + 1) & out_buf_size_mask) || (pOut_buf_next < pOut_buf_start)) { *pIn_buf_size = *pOut_buf_size = 0; return TINFL_STATUS_BAD_PARAM; }

		num_bits = r->m_num_bits; bit_buf = r->m_bit_buf; dist = r->m_dist; counter = r->m_counter; num_extra = r->m_num_extra; dist_from_out_buf_start = r->m_dist_from_out_buf_start;
		TINFL_CR_BEGIN

		bit_buf = num_bits = dist = counter = num_extra = 0;

		do
		{
			if (pIn_buf_cur - pIn_buf_next) { TINFL_CR_RETURN(TINFL_STATE_INDEX_BLOCK_BOUNDRY, TINFL_STATUS_HAS_MORE_OUTPUT); }
			TINFL_GET_BITS(TINFL_STATE_3, r->m_final, 3); r->m_type = r->m_final >> 1;
			if (r->m_type == 0)
			{
				TINFL_SKIP_BITS(TINFL_STATE_5, num_bits & 7);
				for (counter = 0; counter < 4; ++counter) { if (num_bits) TINFL_GET_BITS(TINFL_STATE_6, r->m_raw_header[counter], 8); else TINFL_GET_BYTE(TINFL_STATE_7, r->m_raw_header[counter]); }
				if ((counter = (r->m_raw_header[0] | (r->m_raw_header[1] << 8))) != (Bit32u)(0xFFFF ^ (r->m_raw_header[2] | (r->m_raw_header[3] << 8)))) { TINFL_CR_RETURN_FOREVER(39, TINFL_STATUS_FAILED); }
				while ((counter) && (num_bits))
				{
					TINFL_GET_BITS(TINFL_STATE_51, dist, 8);
					while (pOut_buf_cur >= pOut_buf_end) { TINFL_CR_RETURN(TINFL_STATE_52, TINFL_STATUS_HAS_MORE_OUTPUT); }
					*pOut_buf_cur++ = (Bit8u)dist;
					counter--;
				}
				while (counter)
				{
					size_t n; while (pOut_buf_cur >= pOut_buf_end) { TINFL_CR_RETURN(TINFL_STATE_9, TINFL_STATUS_HAS_MORE_OUTPUT); }
					while (pIn_buf_cur >= pIn_buf_end)
					{
						if (decomp_flags & TINFL_FLAG_HAS_MORE_INPUT)
						{
							TINFL_CR_RETURN(TINFL_STATE_38, TINFL_STATUS_NEEDS_MORE_INPUT);
						}
						else
						{
							TINFL_CR_RETURN_FOREVER(40, TINFL_STATUS_FAILED);
						}
					}
					n = ZIP_MIN(ZIP_MIN((size_t)(pOut_buf_end - pOut_buf_cur), (size_t)(pIn_buf_end - pIn_buf_cur)), counter);
					TINFL_MEMCPY(pOut_buf_cur, pIn_buf_cur, n); pIn_buf_cur += n; pOut_buf_cur += n; counter -= (Bit32u)n;
				}
			}
			else if (r->m_type == 3)
			{
				TINFL_CR_RETURN_FOREVER(10, TINFL_STATUS_FAILED);
			}
			else
			{
				if (r->m_type == 1)
				{
					Bit8u *p = r->m_tables[0].m_code_size; Bit32u i;
					r->m_table_sizes[0] = 288; r->m_table_sizes[1] = 32; TINFL_MEMSET(r->m_tables[1].m_code_size, 5, 32);
					for (i = 0; i <= 143; ++i) { *p++ = 8; } for (; i <= 255; ++i) { *p++ = 9; } for (; i <= 279; ++i) { *p++ = 7; } for (; i <= 287; ++i) { *p++ = 8; }
				}
				else
				{
					for (counter = 0; counter < 3; counter++) { TINFL_GET_BITS(TINFL_STATE_11, r->m_table_sizes[counter], "\05\05\04"[counter]); r->m_table_sizes[counter] += s_min_table_sizes[counter]; }
					TINFL_CLEAR(r->m_tables[2].m_code_size); for (counter = 0; counter < r->m_table_sizes[2]; counter++) { Bit32u s; TINFL_GET_BITS(TINFL_STATE_14, s, 3); r->m_tables[2].m_code_size[s_length_dezigzag[counter]] = (Bit8u)s; }
					r->m_table_sizes[2] = 19;
				}
				for ( ; (int)r->m_type >= 0; r->m_type--)
				{
					int tree_next, tree_cur; tinfl_huff_table *pTable;
					Bit32u i, j, used_syms, total, sym_index, next_code[17], total_syms[16]; pTable = &r->m_tables[r->m_type]; TINFL_CLEAR(total_syms); TINFL_CLEAR(pTable->m_look_up); TINFL_CLEAR(pTable->m_tree);
					for (i = 0; i < r->m_table_sizes[r->m_type]; ++i) total_syms[pTable->m_code_size[i]]++;
					used_syms = 0, total = 0; next_code[0] = next_code[1] = 0;
					for (i = 1; i <= 15; ++i) { used_syms += total_syms[i]; next_code[i + 1] = (total = ((total + total_syms[i]) << 1)); }
					if ((65536 != total) && (used_syms > 1))
					{
						TINFL_CR_RETURN_FOREVER(35, TINFL_STATUS_FAILED);
					}
					for (tree_next = -1, sym_index = 0; sym_index < r->m_table_sizes[r->m_type]; ++sym_index)
					{
						Bit32u rev_code = 0, l, cur_code, code_size = pTable->m_code_size[sym_index]; if (!code_size) continue;
						cur_code = next_code[code_size]++; for (l = code_size; l > 0; l--, cur_code >>= 1) rev_code = (rev_code << 1) | (cur_code & 1);
						if (code_size <= TINFL_FAST_LOOKUP_BITS) { Bit16s k = (Bit16s)((code_size << 9) | sym_index); while (rev_code < TINFL_FAST_LOOKUP_SIZE) { pTable->m_look_up[rev_code] = k; rev_code += (1 << code_size); } continue; }
						if (0 == (tree_cur = pTable->m_look_up[rev_code & (TINFL_FAST_LOOKUP_SIZE - 1)])) { pTable->m_look_up[rev_code & (TINFL_FAST_LOOKUP_SIZE - 1)] = (Bit16s)tree_next; tree_cur = tree_next; tree_next -= 2; }
						rev_code >>= (TINFL_FAST_LOOKUP_BITS - 1);
						for (j = code_size; j > (TINFL_FAST_LOOKUP_BITS + 1); j--)
						{
							tree_cur -= ((rev_code >>= 1) & 1);
							if (!pTable->m_tree[-tree_cur - 1]) { pTable->m_tree[-tree_cur - 1] = (Bit16s)tree_next; tree_cur = tree_next; tree_next -= 2; } else tree_cur = pTable->m_tree[-tree_cur - 1];
						}
						tree_cur -= ((rev_code >>= 1) & 1); pTable->m_tree[-tree_cur - 1] = (Bit16s)sym_index;
					}
					if (r->m_type == 2)
					{
						for (counter = 0; counter < (r->m_table_sizes[0] + r->m_table_sizes[1]); )
						{
							Bit32u s; TINFL_HUFF_DECODE(TINFL_STATE_16, dist, &r->m_tables[2]); if (dist < 16) { r->m_len_codes[counter++] = (Bit8u)dist; continue; }
							if ((dist == 16) && (!counter))
							{
								TINFL_CR_RETURN_FOREVER(17, TINFL_STATUS_FAILED);
							}
							num_extra = "\02\03\07"[dist - 16]; TINFL_GET_BITS(TINFL_STATE_18, s, num_extra); s += "\03\03\013"[dist - 16];
							TINFL_MEMSET(r->m_len_codes + counter, (dist == 16) ? r->m_len_codes[counter - 1] : 0, s); counter += s;
						}
						if ((r->m_table_sizes[0] + r->m_table_sizes[1]) != counter)
						{
							TINFL_CR_RETURN_FOREVER(21, TINFL_STATUS_FAILED);
						}
						TINFL_MEMCPY(r->m_tables[0].m_code_size, r->m_len_codes, r->m_table_sizes[0]); TINFL_MEMCPY(r->m_tables[1].m_code_size, r->m_len_codes + r->m_table_sizes[0], r->m_table_sizes[1]);
					}
				}
				for ( ; ; )
				{
					Bit8u *pSrc;
					for ( ; ; )
					{
						if (((pIn_buf_end_m_4 < pIn_buf_cur)) || ((pOut_buf_end_m_2 < pOut_buf_cur)))
						{
							TINFL_HUFF_DECODE(TINFL_STATE_23, counter, &r->m_tables[0]);
							if (counter >= 256)
								break;
							while (pOut_buf_cur >= pOut_buf_end) { TINFL_CR_RETURN(TINFL_STATE_24, TINFL_STATUS_HAS_MORE_OUTPUT); }
							*pOut_buf_cur++ = (Bit8u)counter;
						}
						else
						{
							int sym2; Bit32u code_len;
							#if MINIZ_HAS_64BIT_REGISTERS
							if (num_bits < 30) { bit_buf |= (((tinfl_bit_buf_t)ZIP_READ_LE32(pIn_buf_cur)) << num_bits); pIn_buf_cur += 4; num_bits += 32; }
							#else
							if (num_bits < 15) { bit_buf |= (((tinfl_bit_buf_t)ZIP_READ_LE16(pIn_buf_cur)) << num_bits); pIn_buf_cur += 2; num_bits += 16; }
							#endif

							sym2 = r_tables_0_look_up[bit_buf & (TINFL_FAST_LOOKUP_SIZE - 1)];
							if (sym2 < 0)
							{
								code_len = TINFL_FAST_LOOKUP_BITS;
								do { sym2 = r->m_tables[0].m_tree[~sym2 + ((bit_buf >> code_len++) & 1)]; } while (sym2 < 0);
							}
							else
								code_len = sym2 >> 9;
							counter = sym2;
							bit_buf >>= code_len;
							num_bits -= code_len;
							if (counter & 256)
								break;

							#if !MINIZ_HAS_64BIT_REGISTERS
							if (num_bits < 15) { bit_buf |= (((tinfl_bit_buf_t)ZIP_READ_LE16(pIn_buf_cur)) << num_bits); pIn_buf_cur += 2; num_bits += 16; }
							#endif

							sym2 = r_tables_0_look_up[bit_buf & (TINFL_FAST_LOOKUP_SIZE - 1)];
							if (sym2 >= 0)
								code_len = sym2 >> 9;
							else
							{
								code_len = TINFL_FAST_LOOKUP_BITS;
								do { sym2 = r->m_tables[0].m_tree[~sym2 + ((bit_buf >> code_len++) & 1)]; } while (sym2 < 0);
							}
							bit_buf >>= code_len;
							num_bits -= code_len;

							pOut_buf_cur[0] = (Bit8u)counter;
							if (sym2 & 256)
							{
								pOut_buf_cur++;
								counter = sym2;
								break;
							}
							pOut_buf_cur[1] = (Bit8u)sym2;
							pOut_buf_cur += 2;
						}
					}
					if ((counter &= 511) == 256) break;

					num_extra = s_length_extra[counter - 257]; counter = s_length_base[counter - 257];
					if (num_extra) { Bit32u extra_bits; TINFL_GET_BITS(TINFL_STATE_25, extra_bits, num_extra); counter += extra_bits; }

					TINFL_HUFF_DECODE(TINFL_STATE_26, dist, &r->m_tables[1]);
					num_extra = s_dist_extra[dist]; dist = s_dist_base[dist];
					if (num_extra) { Bit32u extra_bits; TINFL_GET_BITS(TINFL_STATE_27, extra_bits, num_extra); dist += extra_bits; }

					dist_from_out_buf_start = pOut_buf_cur - pOut_buf_start;
					if ((dist > dist_from_out_buf_start) && (decomp_flags & TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF))
					{
						TINFL_CR_RETURN_FOREVER(37, TINFL_STATUS_FAILED);
					}

					pSrc = pOut_buf_start + ((dist_from_out_buf_start - dist) & out_buf_size_mask);

					if ((ZIP_MAX(pOut_buf_cur, pSrc) + counter) <= pOut_buf_end)
					{
						do
						{
							pOut_buf_cur[0] = pSrc[0];
							pOut_buf_cur[1] = pSrc[1];
							pOut_buf_cur[2] = pSrc[2];
							pOut_buf_cur += 3; pSrc += 3;
						} while ((int)(counter -= 3) > 2);
						if ((int)counter > 0)
						{
							*(pOut_buf_cur++) = pSrc[0];
							if (counter == 2)
								*(pOut_buf_cur++) = pSrc[1];
						}
					}
					else
					{
						while (counter--)
						{
							while (pOut_buf_cur >= pOut_buf_end) { TINFL_CR_RETURN(TINFL_STATE_53, TINFL_STATUS_HAS_MORE_OUTPUT); }
							*pOut_buf_cur++ = pOut_buf_start[(dist_from_out_buf_start++ - dist) & out_buf_size_mask];
						}
					}
				}
			}
		} while (!(r->m_final & 1));
		TINFL_CR_RETURN_FOREVER(34, TINFL_STATUS_DONE);
		TINFL_CR_FINISH

		common_exit:
		r->m_num_bits = num_bits; r->m_bit_buf = bit_buf; r->m_dist = dist; r->m_counter = counter; r->m_num_extra = num_extra; r->m_dist_from_out_buf_start = dist_from_out_buf_start;
		*pIn_buf_size = (Bit32u)(pIn_buf_cur - pIn_buf_next); *pOut_buf_size = (Bit32u)(pOut_buf_cur - pOut_buf_next);
		return status;

		#undef TINFL_MACRO_END
		#undef TINFL_MEMCPY
		#undef TINFL_MEMSET
		#undef TINFL_CR_BEGIN
		#undef TINFL_CR_RETURN
		#undef TINFL_CR_RETURN_FOREVER
		#undef TINFL_CR_FINISH
		#undef TINFL_GET_BYTE
		#undef TINFL_NEED_BITS
		#undef TINFL_SKIP_BITS
		#undef TINFL_GET_BITS
		#undef TINFL_HUFF_BITBUF_FILL
		#undef TINFL_HUFF_DECODE
	}
};

struct oz_unshrink
{
	// BASED ON OZUNSHRINK
	// Ozunshrink / Old ZIP Unshrink (ozunshrink.h) (public domain)
	// By Jason Summers - https://github.com/jsummers/oldunzip

	enum
	{
		OZ_ERRCODE_OK                  = 0,
		OZ_ERRCODE_GENERIC_ERROR       = 1,
		OZ_ERRCODE_BAD_CDATA           = 2,
		OZ_ERRCODE_READ_FAILED         = 6,
		OZ_ERRCODE_WRITE_FAILED        = 7,
		OZ_ERRCODE_INSUFFICIENT_CDATA  = 8,
	};

	Bit8u *out_start, *out_cur, *out_end;
	Bit8u *in_start, *in_cur, *in_end;

	// The code table (implements a dictionary)
	enum { OZ_VALBUFSIZE = 7936, OZ_NUM_CODES = 8192 };
	Bit8u valbuf[OZ_VALBUFSIZE]; // Max possible chain length (8192 - 257 + 1 = 7936)
	struct { Bit16u parent; Bit8u value; Bit8u flags; } ct[OZ_NUM_CODES];

	static int Run(oz_unshrink *oz)
	{
		enum { OZ_INITIAL_CODE_SIZE = 9, OZ_MAX_CODE_SIZE = 13, OZ_INVALID_CODE = 256 };
		Bit32u oz_bitreader_buf = 0;
		Bit8u  oz_bitreader_nbits_in_buf = 0;
		Bit8u  oz_curr_code_size = OZ_INITIAL_CODE_SIZE;
		Bit16u oz_oldcode = 0;
		Bit16u oz_highest_code_ever_used = 0;
		Bit16u oz_free_code_search_start = 257;
		Bit8u  oz_last_value = 0;
		bool   oz_have_oldcode = false;
		bool   oz_was_clear = false;

		memset(oz->ct, 0, sizeof(oz->ct));
		for (Bit16u i = 0; i < 256; i++)
		{
			// For entries <=256, .parent is always set to OZ_INVALID_CODE.
			oz->ct[i].parent = OZ_INVALID_CODE;
			oz->ct[i].value = (Bit8u)i;
		}
		for (Bit16u j = 256; j < OZ_NUM_CODES; j++)
		{
			// For entries >256, .parent==OZ_INVALID_CODE means code is unused
			oz->ct[j].parent = OZ_INVALID_CODE;
		}

		for (;;)
		{
			while (oz_bitreader_nbits_in_buf < oz_curr_code_size)
			{
				if (oz->in_cur >= oz->in_end) return OZ_ERRCODE_INSUFFICIENT_CDATA;
				Bit8u b = *(oz->in_cur++);
				oz_bitreader_buf |= ((Bit32u)b) << oz_bitreader_nbits_in_buf;
				oz_bitreader_nbits_in_buf += 8;
			}

			Bit16u code = (Bit16u)(oz_bitreader_buf & ((1U << oz_curr_code_size) - 1U));
			oz_bitreader_buf >>= oz_curr_code_size;
			oz_bitreader_nbits_in_buf -= oz_curr_code_size;

			if (code == 256)
			{
				oz_was_clear = true;
				continue;
			}

			if (oz_was_clear)
			{
				oz_was_clear = false;

				if (code == 1 && (oz_curr_code_size < OZ_MAX_CODE_SIZE))
				{
					oz_curr_code_size++;
					continue;
				}
				if (code != 2) return OZ_ERRCODE_BAD_CDATA;

				// partial clear
				Bit16u i;
				for (i = 257; i <= oz_highest_code_ever_used; i++)
				{
					if (oz->ct[i].parent != OZ_INVALID_CODE)
					{
						oz->ct[oz->ct[i].parent].flags = 1; // Mark codes that have a child
					}
				}

				for (i = 257; i <= oz_highest_code_ever_used; i++)
				{
					if (oz->ct[i].flags == 0)
					{
						oz->ct[i].parent = OZ_INVALID_CODE; // Clear this code
						oz->ct[i].value = 0;
					}
					else
					{
						oz->ct[i].flags = 0; // Leave all flags at 0, for next time.
					}
				}

				oz_free_code_search_start = 257;
				continue;
			}

			// Process a single (nonspecial) LZW code that was read from the input stream.
			if (code >= OZ_NUM_CODES) return OZ_ERRCODE_GENERIC_ERROR;

			Bit16u emit_code;
			bool late_add, code_is_in_table = (code < 256 || oz->ct[code].parent != OZ_INVALID_CODE);
			if      (!oz_have_oldcode) { late_add = false; goto OZ_EMIT_CODE;         } //emit only
			else if (code_is_in_table) { late_add =  true; goto OZ_EMIT_CODE;         } //emit, then add
			else                       { late_add = false; goto OZ_ADD_TO_DICTIONARY; } //add, then emit

			// Add a code to the dictionary.
			OZ_ADD_TO_DICTIONARY:
			Bit16u newpos, valbuf_pos;
			for (newpos = oz_free_code_search_start; ; newpos++)
			{
				if (newpos >= OZ_NUM_CODES) return OZ_ERRCODE_BAD_CDATA;
				if (oz->ct[newpos].parent == OZ_INVALID_CODE) break;
			}
			oz->ct[newpos].parent = oz_oldcode;
			oz->ct[newpos].value = oz_last_value;
			oz_free_code_search_start = newpos + 1;
			if (newpos > oz_highest_code_ever_used)
			{
				oz_highest_code_ever_used = newpos;
			}
			if (late_add) goto OZ_FINISH_PROCESS_CODE;

			// Decode an LZW code to one or more values, and write the values. Updates oz_last_value.
			OZ_EMIT_CODE:
			for (emit_code = code, valbuf_pos = OZ_VALBUFSIZE;;) // = First entry that's used
			{
				if (emit_code >= OZ_NUM_CODES) return OZ_ERRCODE_GENERIC_ERROR;

				// Check if infinite loop (probably an internal error).
				if (valbuf_pos == 0) return OZ_ERRCODE_GENERIC_ERROR;

				// valbuf is a stack, essentially. We fill it in the reverse direction, to make it simpler to write the final byte sequence.
				valbuf_pos--;

				if (emit_code >= 257 && oz->ct[emit_code].parent == OZ_INVALID_CODE)
				{
					oz->valbuf[valbuf_pos] = oz_last_value;
					emit_code = oz_oldcode;
					continue;
				}

				oz->valbuf[valbuf_pos] = oz->ct[emit_code].value;

				if (emit_code < 257)
				{
					oz_last_value = oz->ct[emit_code].value;

					// Write out the collected values.
					size_t n = OZ_VALBUFSIZE - valbuf_pos;
					if (oz->out_cur + n > oz->out_end) return OZ_ERRCODE_WRITE_FAILED;
					memcpy(oz->out_cur, &oz->valbuf[valbuf_pos], n);
					oz->out_cur += n;
					if (oz->out_cur == oz->out_end) return OZ_ERRCODE_OK;

					break;
				}

				// Traverse the tree, back toward the root codes.
				emit_code = oz->ct[emit_code].parent;
			}
			if (late_add) goto OZ_ADD_TO_DICTIONARY;

			if (!oz_have_oldcode)
			{
				oz_have_oldcode = true;
				oz_last_value = (Bit8u)code;
			}

			OZ_FINISH_PROCESS_CODE:
			oz_oldcode = code;
		}
	}
};

struct unz_explode
{
	// BASED ON INFO-ZIP UNZIP
	// Info-ZIP UnZip v5.4 (explode.c and inflate.c)
	// Put in the public domain by Mark Adler

	enum
	{
		UNZ_ERRCODE_OK                  = 0,
		UNZ_ERRCODE_INCOMPLETE_SET      = 1,
		UNZ_ERRCODE_INVALID_TABLE_INPUT = 2,
		UNZ_ERRCODE_OUTOFMEMORY         = 3,
		UNZ_ERRCODE_INVALID_TREE_INPUT  = 4,
		UNZ_ERRCODE_INTERNAL_ERROR      = 5,
		UNZ_ERRCODE_OUTPUT_ERROR        = 6,
	};

	Bit8u *out_start, *out_cur, *out_end;
	Bit8u *in_start, *in_cur, *in_end;

	enum { WSIZE = 0x8000 }; // window size--must be a power of two
	Bit8u slide[WSIZE];

	static Bit8u GetByte(unz_explode* exploder)
	{
		return (exploder->in_cur < exploder->in_end ? *(exploder->in_cur++) : 0);
	}

	struct huft
	{
		// number of extra bits or operation, number of bits in this code or subcode
		Bit8u e, b;
		// literal, length base, or distance base || pointer to next level of table
		union { Bit16u n; huft *t; } v;
	};

	static void huft_free(huft *t)
	{
		for (huft *p = t, *q; p != (huft *)NULL; p = q)
		{
			q = (--p)->v.t;
			free(p);
		}
	}

	static int get_tree_build_huft(unz_explode* exploder, Bit32u *b, Bit32u n, Bit32u s, const Bit16u *d, const Bit16u *e, huft **t, int *m)
	{
		// Get the bit lengths for a code representation from the compressed stream.
		// If get_tree() returns 4, then there is an error in the data
		Bit32u bytes_remain;    // bytes remaining in list
		Bit32u lengths_entered; // lengths entered
		Bit32u ncodes;  // number of codes
		Bit32u bitlen; // bit length for those codes

		// get bit lengths
		bytes_remain = (Bit32u)GetByte(exploder) + 1; // length/count pairs to read
		lengths_entered = 0; // next code
		do
		{
			bitlen = ((ncodes = (Bit32u)GetByte(exploder)) & 0xf) + 1; //bits in code (1..16)
			ncodes = ((ncodes & 0xf0) >> 4) + 1; // codes with those bits (1..16)
			if (lengths_entered + ncodes > n) return UNZ_ERRCODE_INVALID_TREE_INPUT; // don't overflow bit_lengths
			do
			{
				b[lengths_entered++] = bitlen;
			} while (--ncodes);
		} while (--bytes_remain);
		if (lengths_entered != n) return UNZ_ERRCODE_INVALID_TREE_INPUT;

		// Mystery code, the original (huft_build function) wasn't much more readable IMHO (see inflate.c)
		// Given a list of code lengths and a maximum table size, make a set of tables to decode that set of codes.  Return zero on success, one if
		// the given code set is incomplete (the tables are still built in this case), two if the input is invalid (all zero length codes or an
		// oversubscribed set of lengths), and three if not enough memory.
		enum { BMAX = 16, N_MAX = 288 }; Bit32u a, c[BMAX + 1], f, i, j, *p, v[N_MAX], x[BMAX + 1], *xp, z; int g, h, k, l, w, y; huft *q, r, *u[BMAX];
		memset(c, 0, sizeof(c)); p = b; i = n; do { c[*p++]++; } while (--i); if (c[0] == n) { *t = (huft *)NULL; *m = 0; return UNZ_ERRCODE_OK; }
		l = *m; for (j = 1; j <= BMAX; j++) if (c[j]) break; k = j; if ((Bit32u)l < j) l = j; for (i = BMAX; i; i--) if (c[i]) break;
		g = i; if ((Bit32u)l > i) l = i; *m = l; for (y = 1 << j; j < i; j++, y <<= 1) if ((y -= c[j]) < 0) return UNZ_ERRCODE_INVALID_TABLE_INPUT;
		if ((y -= c[i]) < 0) { return UNZ_ERRCODE_INVALID_TABLE_INPUT; } c[i] += y; x[1] = j = 0; p = c + 1; xp = x + 2; while (--i) { *xp++ = (j += *p++); }
		p = b; i = 0; do { if ((j = *p++) != 0) v[x[j]++] = i; } while (++i < n); x[0] = i = 0; p = v; h = -1; w = -l;
		u[0] = (huft *)NULL; q = (huft *)NULL; z = 0; for (; k <= g; k++) { a = c[k]; while (a--) { while (k > w + l)
		{ h++; w += l; z = (z = g - w) > (Bit32u)l ? l : z; if ((f = 1 << (j = k - w)) > a + 1) { f -= a + 1; xp = c + k; while (++j < z)
		{ if ((f <<= 1) <= *++xp) break; f -= *xp; } } z = 1 << j; if ((q = (huft *)malloc((z + 1)*sizeof(huft))) == (huft *)NULL)
		{ if (h) huft_free(u[0]); return UNZ_ERRCODE_OUTOFMEMORY; } *t = q + 1; *(t = &(q->v.t)) = (huft *)NULL; u[h] = ++q; if (h)
		{ x[h] = i; r.b = (Bit8u)l; r.e = (Bit8u)(16 + j); r.v.t = q; j = i >> (w - l); u[h - 1][j] = r; } } r.b = (Bit8u)(k - w); if (p >= v + n) r.e = 99; else if (*p < s)
		{ r.e = (Bit8u)(*p < 256 ? 16 : 15); r.v.n = (Bit16u)*p++; } else
		{ r.e = (Bit8u)e[*p - s]; r.v.n = d[*p++ - s]; } f = 1 << (k - w); for (j = i >> w; j < z; j += f) q[j] = r; for (j = 1 << (k - 1);
		i & j; j >>= 1) { i ^= j; } i ^= j; while ((i & ((1 << w) - 1)) != x[h]) { h--; w -= l; } } }
		return (y == 0 || g == 1 ? UNZ_ERRCODE_OK : UNZ_ERRCODE_INCOMPLETE_SET);
	}

	static int flush(unz_explode* exploder, Bit32u w)
	{
		Bit8u *out_w = exploder->out_cur + w;
		int ret = (out_w > exploder->out_end ? 1 : 0);
		if (ret) out_w = exploder->out_end;
		memcpy(exploder->out_cur, exploder->slide, (out_w - exploder->out_cur));
		exploder->out_cur = out_w;
		return ret;
	}

	static int Run(unz_explode* exploder, Bit16u zip_bit_flag)
	{
		// Tables for length and distance
		static const Bit16u cplen2[]    = { 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65 };
		static const Bit16u cplen3[]    = { 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66 };
		static const Bit16u extra[]     = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8 };
		static const Bit16u cpdist4[]   = { 1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705, 769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473, 1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177, 2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881, 2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585, 3649, 3713, 3777, 3841, 3905, 3969, 4033 };
		static const Bit16u cpdist8[]   = { 1, 129, 257, 385, 513, 641, 769, 897, 1025, 1153, 1281, 1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689, 2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097, 4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505, 5633, 5761, 5889, 6017, 6145, 6273, 1, 6529, 6657, 6785, 6913, 7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065 };
		static const Bit16u mask_bits[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff, 0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff };

		huft *tb = NULL, *tl = NULL, *td = NULL; // literal code, length code, distance code tables
		Bit32u l[256]; // bit lengths for codes
		bool is8k  = ((zip_bit_flag & 2) == 2), islit = ((zip_bit_flag & 4) == 4);
		int bb = (islit ? 9 : 0), bl = 7, bd = ((exploder->in_end - exploder->in_start)  > 200000 ? 8 : 7); // bits for tb, tl, td
		Bit32u numbits = (is8k ? 7 : 6);

		int r;
		if (islit && (r = get_tree_build_huft(exploder, l, 256, 256, NULL, NULL, &tb, &bb)) != 0) goto done;
		if ((r = get_tree_build_huft(exploder, l, 64, 0, (islit ? cplen3 : cplen2), extra, &tl, &bl)) != 0) goto done;
		if ((r = get_tree_build_huft(exploder, l, 64, 0, (is8k ? cpdist8 : cpdist4), extra, &td, &bd)) != 0) goto done;

		// The implode algorithm uses a sliding 4K or 8K byte window on the uncompressed stream to find repeated byte strings.
		// This is implemented here as a circular buffer. The index is updated simply by incrementing and then and'ing with 0x0fff (4K-1) or 0x1fff (8K-1).
		// Here, the 32K buffer of inflate is used, and it works just as well to always have a 32K circular buffer, so the index is anded with 0x7fff.
		// This is done to allow the window to also be used as the output buffer.
		Bit32u s;          // bytes to decompress
		Bit32u e;          // table entry flag/number of extra bits
		Bit32u n, d;       // length and index for copy
		Bit32u w;          // current window position
		Bit32u mb, ml, md; // masks for bb (if lit), bl and bd bits
		Bit32u b;          // bit buffer
		Bit32u k;          // number of bits in bit buffer
		Bit32u u;          // true if unflushed
		huft *t;           // pointer to table entry

		#define UNZ_NEEDBITS(n) do {while(k<(n)){b|=((Bit32u)GetByte(exploder))<<k;k+=8;}} while(0)
		#define UNZ_DUMPBITS(n) do {b>>=(n);k-=(n);} while(0)

		// explode the coded data
		b = k = w = 0; // initialize bit buffer, window
		u = 1;         // buffer unflushed

		// precompute masks for speed
		mb = mask_bits[bb];
		ml = mask_bits[bl];
		md = mask_bits[bd];
		s = (Bit32u)(exploder->out_end - exploder->out_start);
		while (s > 0) // do until ucsize bytes uncompressed
		{
			UNZ_NEEDBITS(1);
			if (b & 1) // then literal
			{
				UNZ_DUMPBITS(1);
				s--;
				if (tb)
				{
					// LIT: Decompress the imploded data using coded literals and an 8K sliding window.
					UNZ_NEEDBITS((Bit32u)bb); // get coded literal
					if ((e = (t = tb + ((~(Bit32u)b) & mb))->e) > 16)
					{
						do
						{
							if (e == 99) { r = UNZ_ERRCODE_INTERNAL_ERROR; goto done; }
							UNZ_DUMPBITS(t->b);
							e -= 16;
							UNZ_NEEDBITS(e);
						} while ((e = (t = t->v.t + ((~(Bit32u)b) & mask_bits[e]))->e) > 16);
					}
					UNZ_DUMPBITS(t->b);
					exploder->slide[w++] = (Bit8u)t->v.n;
					if (w == WSIZE) { if (flush(exploder, w)) { r = UNZ_ERRCODE_OUTPUT_ERROR; goto done; } w = u = 0; }
				}
				else
				{
					// UNLIT: Decompress the imploded data using uncoded literals and an 8K sliding window.
					UNZ_NEEDBITS(8);
					exploder->slide[w++] = (Bit8u)b;
					if (w == WSIZE) { if (flush(exploder, w)) { r = UNZ_ERRCODE_OUTPUT_ERROR; goto done; } w = u = 0; }
					UNZ_DUMPBITS(8);
				}
			}
			else // else distance/length
			{
				UNZ_DUMPBITS(1);
				UNZ_NEEDBITS(numbits); // get distance low bits
				d = (Bit32u)b & ((1 << numbits) - 1);
				UNZ_DUMPBITS(numbits);
				UNZ_NEEDBITS((Bit32u)bd); // get coded distance high bits
				if ((e = (t = td + ((~(Bit32u)b) & md))->e) > 16)
				{
					do
					{
						if (e == 99) { r = UNZ_ERRCODE_INTERNAL_ERROR; goto done; }
						UNZ_DUMPBITS(t->b);
						e -= 16;
						UNZ_NEEDBITS(e);
					} while ((e = (t = t->v.t + ((~(Bit32u)b) & mask_bits[e]))->e) > 16);
				}
				UNZ_DUMPBITS(t->b);
				d = w - d - t->v.n; // construct offset
				UNZ_NEEDBITS((Bit32u)bl); // get coded length
				if ((e = (t = tl + ((~(Bit32u)b) & ml))->e) > 16)
				{
					do
					{
						if (e == 99) { r = UNZ_ERRCODE_INTERNAL_ERROR; goto done; }
						UNZ_DUMPBITS(t->b);
						e -= 16;
						UNZ_NEEDBITS(e);
					} while ((e = (t = t->v.t + ((~(Bit32u)b) & mask_bits[e]))->e) > 16);
				}
				UNZ_DUMPBITS(t->b);
				n = t->v.n;
				if (e) // get length extra bits
				{
					UNZ_NEEDBITS(8);
					n += (Bit32u)b & 0xff;
					UNZ_DUMPBITS(8);
				}

				// do the copy
				s -= n;
				do
				{
					n -= (e = (e = WSIZE - ((d &= WSIZE - 1) > w ? d : w)) > n ? n : e);
					if (u && w <= d)
					{
						memset(exploder->slide + w, 0, e);
						w += e;
						d += e;
					}
					else if (w - d >= e) // (this test assumes unsigned comparison)
					{
						memcpy(exploder->slide + w, exploder->slide + d, e);
						w += e;
						d += e;
					}
					else // do it slow to avoid memcpy() overlap
					{
						do {
							exploder->slide[w++] = exploder->slide[d++];
						} while (--e);
					}
					if (w == WSIZE)
					{
						if (flush(exploder, w)) { r = UNZ_ERRCODE_OUTPUT_ERROR; goto done; }
						w = u = 0;
					}
				} while (n);
			}
		}

		#undef UNZ_NEEDBITS
		#undef UNZ_DUMPBITS

		// flush out slide
		if (flush(exploder, w)) { r = UNZ_ERRCODE_OUTPUT_ERROR; goto done; }

		done:
		huft_free(td);
		huft_free(tl);
		huft_free(tb);
		return r;
	}
};

bool SFileZip::Unpack(Bit64u unpack_until)
{
	ZIP_ASSERT(size && method != METHOD_STORED);
	if (!lhskip && !SkipLocalHeader()) return false;
	SFile& fi = reader.archive;
	if (method == METHOD_DEFLATED)
	{
		struct deflate_state { miniz::tinfl_decompressor inflator; Bit8u read_buf[miniz::MZ_ZIP_MAX_IO_BUF_SIZE]; Bit32u read_buf_avail, read_buf_ofs, comp_remaining; } *st = (deflate_state*)decomp_state;
		if (!st)
		{
			decomp_state = st = (deflate_state*)malloc(sizeof(deflate_state));
			miniz::tinfl_init(&st->inflator);
			st->read_buf_avail = st->read_buf_ofs = 0;
			st->comp_remaining = comp_size;
		}

		//printf("Need Unpacking %s until %u ...\n", path.c_str(), (unsigned)unpack_until);
		const Bit64u want_until = unpack_until + 1 * 1024 * 1024, fiseek = data_ofs + comp_size - st->comp_remaining;
		for (unpack_until = unpacked; unpack_until < want_until;) unpack_until += 8*1024*1024;
		if (unpack_until > size) unpack_until = size;
		if (fi.Seek(fiseek) != fiseek) { ZIP_ASSERT(false); }
		buf = (Bit8u*)realloc(buf, (size_t)unpack_until);
		//printf("Unpacking %s until %u ...\n", path.c_str(), (unsigned)unpack_until);

		for (miniz::tinfl_status status = miniz::TINFL_STATUS_NEEDS_MORE_INPUT; (status == miniz::TINFL_STATUS_NEEDS_MORE_INPUT || status == miniz::TINFL_STATUS_HAS_MORE_OUTPUT) && unpacked != unpack_until;)
		{
			if (!st->read_buf_avail)
			{
				st->read_buf_avail = (st->comp_remaining < miniz::MZ_ZIP_MAX_IO_BUF_SIZE ? st->comp_remaining : miniz::MZ_ZIP_MAX_IO_BUF_SIZE);
				if (fi.Read(st->read_buf, st->read_buf_avail) != st->read_buf_avail)
					break;
				st->comp_remaining -= st->read_buf_avail;
				st->read_buf_ofs = 0;
			}
			Bit32u out_buf_size = (Bit32u)(unpack_until - unpacked);
			Bit8u *pWrite_buf_cur = buf + unpacked;
			Bit32u in_buf_size = st->read_buf_avail;
			status = miniz::tinfl_decompress(&st->inflator, st->read_buf + st->read_buf_ofs, &in_buf_size, buf, pWrite_buf_cur, &out_buf_size, miniz::TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF | (st->comp_remaining ? miniz::TINFL_FLAG_HAS_MORE_INPUT : 0));
			st->read_buf_avail -= in_buf_size;
			st->read_buf_ofs += in_buf_size;
			unpacked += out_buf_size;
			ZIP_ASSERT(!out_buf_size || unpacked <= unpack_until);
			ZIP_ASSERT(status == miniz::TINFL_STATUS_NEEDS_MORE_INPUT || status == miniz::TINFL_STATUS_HAS_MORE_OUTPUT || status == miniz::TINFL_STATUS_DONE);
		}
		if (unpacked == size) { free(decomp_state); decomp_state = NULL; }
	}
	else if (method == METHOD_SHRUNK)
	{
		ZIP_ASSERT(buf == NULL);
		buf = (Bit8u*)malloc((size_t)size);
		oz_unshrink *unshrink = (oz_unshrink*)malloc(sizeof(oz_unshrink) + comp_size);
		Bit8u* in_buf = (Bit8u*)(unshrink + 1);
		if (fi.Seek(data_ofs) != data_ofs || fi.Read(in_buf, comp_size) != comp_size) goto bad;
		unshrink->in_start = unshrink->in_cur = in_buf;
		unshrink->in_end = in_buf + comp_size;
		unshrink->out_start = unshrink->out_cur = buf;
		unshrink->out_end = unshrink->out_start + size;
		#ifndef NDEBUG
		int res =
		#endif
		oz_unshrink::Run(unshrink);
		ZIP_ASSERT(res == 0);
		free(unshrink);
		unpacked = size;
	}
	else if (method == METHOD_IMPLODED)
	{
		ZIP_ASSERT(buf == NULL);
		buf = (Bit8u*)malloc((size_t)size);
		unz_explode *explode = (unz_explode*)malloc(sizeof(unz_explode) + comp_size);
		Bit8u* in_buf = (Bit8u*)(explode + 1);
		if (fi.Seek(data_ofs) != data_ofs || fi.Read(in_buf, comp_size) != comp_size) goto bad;
		explode->in_start = explode->in_cur = in_buf;
		explode->in_end = in_buf + comp_size;
		explode->out_start = explode->out_cur = buf;
		explode->out_end = explode->out_start + size;
		#ifndef NDEBUG
		int res =
		#endif
		unz_explode::Run(explode, bit_flags);
		ZIP_ASSERT(res == 0);
		free(explode);
		unpacked = size;
	}
	else { bad: ZIP_ASSERT(false); size = 0; return false; }
	return true;
}
